# How to Verify HBS2 Mirror Synchronization

This guide shows various methods to verify that a remote peer has successfully synchronized your repository.

## Quick Verification Methods

### Method 1: Check Using git-hbs2 (Recommended)

On the **remote mirror node**:

```bash
# Install hbs2-git3 tools if not already installed
nix develop github:NCrashed/hbs2/dev-0.25.3

# Check if repository manifest is available
git hbs2 repo:manifest 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC

# Should output the manifest:
# (manifest
#   (hbs2-git 3)
#   (seed 12345)
#   (public)
#   (reflog "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP")
# )
```

**If you see the manifest** → Repository metadata is synced ✅

**If you see an error** → Repository hasn't synced yet ❌

---

### Method 2: Check Repository Head

On the **remote mirror node**:

```bash
# Check current repository head (LWWRef value)
git hbs2 repo:head 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC

# Should output a hash:
# Just (HashRef "abc123def456...")
```

Compare with your **local node**:

```bash
git hbs2 repo:head phrase-demise
```

**If hashes match** → LWWRef is synced ✅

---

### Method 3: Check Imported Checkpoint

On the **remote mirror node**:

```bash
# Check what checkpoint was imported from reflog
git hbs2 repo:imported 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC

# Should output checkpoint number:
# 42
```

This shows the last checkpoint number that was imported from the RefLog.

---

### Method 4: Check Git References

On the **remote mirror node**:

```bash
# List all imported git refs (branches, tags)
git hbs2 repo:refs 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC

# Should output:
# abc123def456         refs/heads/master
# 789abc012def         refs/heads/dev
# fedcba987654         refs/tags/v1.0.0
```

**If you see your branches** → Git data is synced ✅

Compare with your **local node**:

```bash
git hbs2 repo:refs phrase-demise
```

**If refs match** → Repository is fully synced ✅

---

### Method 5: Check Object Index Count

On the **remote mirror node**:

```bash
# Count indexed objects
git hbs2 repo:index:count 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC

# Output:
# 15234
```

Compare with your **local node**:

```bash
git hbs2 repo:index:count phrase-demise
```

**If counts are similar** → Most objects are synced ✅

**Note:** Counts might differ slightly due to timing, but should be within ~1% of each other.

---

## Detailed Verification

### Check Transaction List

On the **remote mirror node**:

```bash
# List all imported transactions
git hbs2 repo:tx:list:imported 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC

# Output shows segments (S) and checkpoints (C):
# S abc123def456...  segment_hash_1
# S def456abc123...  segment_hash_2
# C ghi789jkl012...  checkpoint_hash  42
```

This shows:
- **S** = Segment (contains git objects)
- **C** = Checkpoint (repository state snapshot)

**More transactions = more data synced**

---

### Check Peer Logs

On the **remote mirror node**:

```bash
# Check hbs2-peer logs for sync activity
sudo journalctl -u hbs2-peer | grep -i "3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"

# Should see:
# poll lwwref 1 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC
# downloading block abc123...
# stored block abc123...
```

**If you see "downloading" messages** → Sync in progress ⏳

**If you see "stored" messages** → Blocks are being saved ✅

---

### Check Storage Directory

On the **remote mirror node**:

```bash
# Check storage size
du -sh /var/lib/hbs2-peer

# Output:
# 2.5G    /var/lib/hbs2-peer
```

**Larger size = more data synced**

**Note:** Initial sync of a large repository may take time.

---

## Monitoring Sync Progress

### Watch Logs in Real-Time

```bash
# Follow logs with filtering
sudo journalctl -u hbs2-peer -f | grep -i "download\|block\|fetch"
```

You should see messages like:
```
[info] downloading block abc123def456...
[info] stored block abc123def456...
[info] fetch completed for segment xyz789...
```

### Check Sync Status Every Minute

```bash
# Create a monitoring script
watch -n 60 'echo "=== Object Count ===" && \
             git hbs2 repo:index:count 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC && \
             echo "\n=== Checkpoint ===" && \
             git hbs2 repo:imported 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC'
```

---

## Complete Verification Checklist

Run these commands on the **remote mirror node**:

```bash
#!/bin/bash
# verify-mirror.sh - Complete mirror verification script

REPO_KEY="3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"

echo "=== HBS2 Mirror Verification ==="
echo "Repository: $REPO_KEY"
echo ""

echo "1. Checking manifest..."
if git hbs2 repo:manifest "$REPO_KEY" &>/dev/null; then
    echo "   ✅ Manifest available"
    git hbs2 repo:manifest "$REPO_KEY" | head -5
else
    echo "   ❌ Manifest not found - repository not synced yet"
    exit 1
fi
echo ""

echo "2. Checking repository head..."
HEAD=$(git hbs2 repo:head "$REPO_KEY")
if [ -n "$HEAD" ]; then
    echo "   ✅ Head: $HEAD"
else
    echo "   ❌ Head not set"
fi
echo ""

echo "3. Checking imported checkpoint..."
CHECKPOINT=$(git hbs2 repo:imported "$REPO_KEY")
echo "   Checkpoint: $CHECKPOINT"
echo ""

echo "4. Checking git references..."
REFS=$(git hbs2 repo:refs "$REPO_KEY" | wc -l)
if [ "$REFS" -gt 0 ]; then
    echo "   ✅ Found $REFS references"
    git hbs2 repo:refs "$REPO_KEY" | head -10
else
    echo "   ❌ No references found"
fi
echo ""

echo "5. Checking object index..."
COUNT=$(git hbs2 repo:index:count "$REPO_KEY")
echo "   ✅ Indexed objects: $COUNT"
echo ""

echo "6. Checking storage size..."
STORAGE_SIZE=$(du -sh /var/lib/hbs2-peer | cut -f1)
echo "   Storage used: $STORAGE_SIZE"
echo ""

echo "=== Verification Complete ==="
if [ "$REFS" -gt 0 ] && [ "$COUNT" -gt 0 ]; then
    echo "✅ Mirror is successfully synced!"
else
    echo "⚠️  Mirror sync in progress or incomplete"
fi
```

Save as `verify-mirror.sh`, make executable, and run:

```bash
chmod +x verify-mirror.sh
./verify-mirror.sh
```

---

## Testing from Third Node

The ultimate test is to try cloning from the mirror:

### On a third machine:

```bash
# Add mirror as known peer
# (in hbs2-peer config or as argument)
known-peer "mirror-ip:7354"

# Try to clone
git clone hbs23://3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC test-repo

# Check logs to see where data came from
sudo journalctl -u hbs2-peer | grep -i "receive\|peer"
```

**If clone succeeds** → Mirror is working and distributing! ✅

---

## Comparing Local vs Mirror

Create this script to compare local and mirror:

```bash
#!/bin/bash
# compare-repos.sh

LOCAL_REMOTE="phrase-demise"
MIRROR_REPO="3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"

echo "=== Comparing Local vs Mirror ==="
echo ""

echo "Local HEAD:"
git hbs2 repo:head "$LOCAL_REMOTE"
echo ""

echo "Mirror HEAD (run on mirror node):"
echo "git hbs2 repo:head \"$MIRROR_REPO\""
echo ""

echo "Local Objects:"
git hbs2 repo:index:count "$LOCAL_REMOTE"
echo ""

echo "Mirror Objects (run on mirror node):"
echo "git hbs2 repo:index:count \"$MIRROR_REPO\""
echo ""

echo "Local Refs:"
git hbs2 repo:refs "$LOCAL_REMOTE" | sort
echo ""

echo "Mirror Refs (run on mirror node):"
echo "git hbs2 repo:refs \"$MIRROR_REPO\" | sort"
```

---

## Troubleshooting Sync Issues

### Problem: Manifest Not Found

```bash
git hbs2 repo:manifest 3XgC98VY...
# Error: manifest not found
```

**Cause:** LWWRef hasn't synced yet

**Solutions:**

1. Check peer logs:
   ```bash
   sudo journalctl -u hbs2-peer | grep -i "lwwref\|3XgC98"
   ```

2. Verify poll configuration:
   ```bash
   cat /etc/hbs2-peer/config | grep -i poll
   ```

3. Check peer connectivity:
   ```bash
   sudo journalctl -u hbs2-peer | grep -i "peer\|bootstrap"
   ```

4. Wait longer (initial sync can take time)

---

### Problem: Refs Not Showing

```bash
git hbs2 repo:refs 3XgC98VY...
# (empty output)
```

**Cause:** RefLog transactions not imported yet

**Solutions:**

1. Check reflog subscription:
   ```bash
   cat /etc/hbs2-peer/config | grep reflog
   ```

2. Check reflog in manifest:
   ```bash
   git hbs2 repo:manifest 3XgC98VY... | grep reflog
   ```

3. Rebuild index:
   ```bash
   git hbs2 repo:index:build 3XgC98VY...
   ```

---

### Problem: Object Count Too Low

```bash
git hbs2 repo:index:count 3XgC98VY...
# Output: 10
# (Expected: 50000)
```

**Cause:** Sync still in progress

**Solutions:**

1. Monitor download progress:
   ```bash
   sudo journalctl -u hbs2-peer -f | grep -i download
   ```

2. Check for errors:
   ```bash
   sudo journalctl -u hbs2-peer | grep -i error
   ```

3. Check network connectivity to source peers:
   ```bash
   # Add source peer as known peer
   known-peer "source-peer-ip:7354"
   ```

---

## Expected Sync Times

| Repository Size | Object Count | Expected Time* |
|----------------|--------------|----------------|
| Small (<10MB)  | <1,000       | 1-5 minutes    |
| Medium (100MB) | 10,000       | 10-30 minutes  |
| Large (1GB)    | 100,000      | 1-3 hours      |
| Huge (10GB+)   | 1,000,000+   | 6-24 hours     |

\* Times vary based on network speed and peer availability

---

## NixOS Integration

For automated verification, add to your NixOS config:

```nix
systemd.timers.verify-hbs2-mirror = {
  wantedBy = [ "timers.target" ];
  timerConfig = {
    OnCalendar = "hourly";
    Persistent = true;
  };
};

systemd.services.verify-hbs2-mirror = {
  description = "Verify HBS2 Mirror Sync";
  serviceConfig = {
    Type = "oneshot";
    ExecStart = pkgs.writeScript "verify-mirror" ''
      #!/bin/sh
      REPO="3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"

      # Check object count
      COUNT=$(${config.services.hbs2-peer.package}/bin/git-hbs2 repo:index:count "$REPO" 2>/dev/null || echo "0")

      echo "Mirror sync status: $COUNT objects"

      # Alert if count is too low (adjust threshold as needed)
      if [ "$COUNT" -lt 1000 ]; then
        echo "WARNING: Object count too low!"
      fi
    '';
  };
};
```

---

## Summary: Quick Check Commands

```bash
# On remote mirror node

# 1. Basic check - is manifest available?
git hbs2 repo:manifest YOUR_REPO_KEY

# 2. Check sync status
git hbs2 repo:imported YOUR_REPO_KEY
git hbs2 repo:index:count YOUR_REPO_KEY

# 3. Check git refs
git hbs2 repo:refs YOUR_REPO_KEY

# 4. Monitor logs
sudo journalctl -u hbs2-peer -f | grep -i "download\|YOUR_REPO_KEY"
```

**If all commands return data → Mirror is synced! ✅**
