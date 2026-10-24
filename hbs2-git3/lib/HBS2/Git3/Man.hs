module HBS2.Git3.Man where

import HBS2.Git3.Prelude
import Data.Config.Suckless.Script

import Text.InterpolatedString.Perl6 (qc)

manRemotes :: MakeDictM c m () -> MakeDictM c m ()
manRemotes = brief "show known hbs2-git remotes"


manInit :: MakeDictM c m () -> MakeDictM c m ()
manInit = brief "initializes a new repository, or publishes one again under its key"
    . args [ arg "new repository flag"                     "--new"
           , arg "group key hash for encrypted repository" "<--encrypted group-key>"
           , arg "replace an unreadable value"              "--force"
           , arg "hbs23:// url, key or git remote"          "[repo]"
           ]
    . desc initDesc
    . examples initExamples

  where
    initDesc = vcat
      [ "Without a repository, creates a new key and publishes an empty"
      , "repository under it."
      , ""
      , "With one, publishes an empty repository under THAT key, so the URL"
      , "every clone already has keeps working: the way back for a repository"
      , "whose lwwref was lost. It needs the key's private key in hbs2-keyman."
      , ""
      , "It waits for the key's current value first (HBS2_GIT_REPO_WAIT"
      , "seconds, 60 by default), and:"
      , "  - no value: publishes a new manifest and reflog;"
      , "  - a readable value: changes nothing, and says so;"
      , "  - a value whose manifest cannot be read: refuses, unless --force."
      , ""
      , "Then push everything again: git push <remote> --all; git push <remote> --tags."
      , "Clones need nothing else: local state is kept per reflog, and the new"
      , "repository has a new reflog, so what a clone kept about the lost one"
      , "is not read again. The old .hbs2-git3/<reflog> directory can be removed."
      ]

    initExamples = [qc|
; just init a new repository

hbs2-git init --new

; init encrypted repository

; create new group key:
; your real keys will be different
; all hashes/keys appear in exampels/logs a PUBLIC information,
; so no secrets disclosures.

hbs2-cli hbs2:groupkey:store [hbs2:groupkey:create 67CRxnoQWasQsY9iidjJDYXSTKEZkpSVgDQYweWuhfd3]
39baH7SqqsAGgCSr3k9RJgY4nTwiMRXrgZUmKPFndzn8


hbs2-git init  --new --encrypted 39baH7SqqsAGgCSr3k9RJgY4nTwiMRXrgZUmKPFndzn8

added git remote laundry-worry hbs23://7F1D7QGVVwJFJ649dsSHgrDUuqHYti3nkFx<censored>
updateRepoKey 7F1D7QGVVwJFJ649dsSHgrDUuqHYti3nkFx<censored>

git remote

laundry-worry
^^^^^^^^^^^^^

This is the git remote for the new repo. Rename it if you want.

hbs2-git3  repo:remotes
7F1D7QGVVwJFJ649dsSHgrDUuqHYti3nkFx<censored> laundry-worry


; publish a lost repository again under the key it had
; (a key, an hbs23:// URL or the name of a git remote)

hbs2-git3 repo:init origin
git push origin --all
git push origin --tags


  |]

manGitListObjectsNew :: MakeDictM c m () -> MakeDictM c m ()
manGitListObjectsNew =
  brief "lists new git objects"
  . args [ arg "hash|name"   "remote"
         , arg "(-r rev)?"   "git revision"
         ]

manRepoRelayOnly ::  MakeDictM c m () -> MakeDictM c m ()
manRepoRelayOnly =  brief "subscribe hbs2-peer to repository references (lwwref+reflog)"
                   . desc description
                   . args [ arg "public-key" "lwwref"]

  where
    description = vcat [
          "useful when you want hbs2-peer to distribute and backup"
       <> "the repository data without git fetching/cloning"
                ]

