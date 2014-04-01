{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Remotion.Util.Prelude

import {-@ HTF_TESTS @-} HTFTestSuite.CommunicationTests

main = htfMain $ htf_thisModulesTests : htf_importedTests
