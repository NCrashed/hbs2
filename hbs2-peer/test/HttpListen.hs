-- | Where the HTTP API ends up listening, given a config.
--
-- The API authenticates nobody: everything it serves, it serves to whoever
-- opened the port. It used to bind every interface whenever the config said
-- nothing, which is the one case where nobody chose it. The default here is
-- the security property, so it is spelled out as a test rather than left to
-- be re-derived from the fall-through in `cfgValue`.
module HttpListen (httpListenTests) where

import PeerConfig

import Control.Monad.Reader
import Data.Either

import Test.Tasty
import Test.Tasty.HUnit

resolve :: String -> Maybe (String, Integer)
resolve src = runReader peerHttpListen (fromRight mempty (parseTop src))

httpListenTests :: TestTree
httpListenTests = testGroup "http listen address"
  [ testCase "a config that says nothing keeps the api on this host" do
      resolve "" @?= Just ("127.0.0.1", 5005)

  , testCase "http-port names the port and nothing else" do
      resolve "http-port 4017" @?= Just ("127.0.0.1", 4017)

  , testCase "http-listen is what opens it to the network" do
      resolve "http-listen \"0.0.0.0\"" @?= Just ("0.0.0.0", 5005)

  , testCase "the two clauses compose, in either order" do
      resolve "http-port 4017\nhttp-listen \"0.0.0.0\""
        @?= Just ("0.0.0.0", 4017)
      resolve "http-listen \"0.0.0.0\"\nhttp-port 4017"
        @?= Just ("0.0.0.0", 4017)

  , testCase "off means off, whatever the address says" do
      resolve "http-port \"off\"" @?= Nothing
      resolve "http-port \"off\"\nhttp-listen \"0.0.0.0\"" @?= Nothing

  , testCase "a host name binds where it is written" do
      resolve "http-listen \"192.168.1.10\"" @?= Just ("192.168.1.10", 5005)

  -- What peer-meta consults before announcing a port: a neighbour cannot
  -- open a loopback one, so announcing it only buys us their probes.
  , testCase "loopback is recognised by every spelling that means it" do
      map isLoopbackHost ["127.0.0.1", "::1", "localhost"]
        @?= [True, True, True]

  , testCase "an address that reaches the network is not loopback" do
      map isLoopbackHost ["0.0.0.0", "*", "*4", "192.168.1.10"]
        @?= [False, False, False, False]
  ]
