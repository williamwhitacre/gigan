{--

Copyright (c) 2016, William Whitacre
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the
distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

--}


module Gigan.Knowledge

  (Knowledge,
  KnowledgeBase,
  KnowledgeRecord,
  KnowledgeBaseDelta,

  BaseKnowledge,
  RecordKnowledge,

  Remote, RemoteMap,

  RemoteConfig, RemoteMapConfig,
  QueryTask,

  knowledgeOf, forbiddenKnowledge, pendingKnowledge, undecidedKnowledge,
  unknownKnowledge, voidKnowledge, knowledgeDo,

  maybeOr, resultOr,

  assumeIf, assumeIfNot, assumeIfNow, assumeInCase, assumeInCaseNow,
  decideBy, doOperation, maybeKnownNow, otherwise,

  therefore, within,

  dispatchIf, dispatchIfNot, dispatchInCase,

  isUnknown, isNotUnknown,
  isPending, isNotPending,
  isUndecided, isNotUndecided,
  isForbidden, isNotForbidden,
  isVoid, isNotVoid,
  isNil, isNotNil,
  isKnown, isNotKnown,
  isOperation, isNotOperation,

  knowledgeIntegrate, knowledgeQuery, knowledgeUpdate,

  baseDeltaMap, baseDeltaTherefore,

  base, baseAt, baseDo, baseErrorHandler, baseIntegrate,
  baseMember, baseQuery, baseUpdate,

  record, recordAt, recordBinding, recordContent, recordDo, recordErrorHandler, recordField,
  recordIntegrate, recordQuery, recordUpdate, reduceNotKnownNowTo,

  remoteConfig,
  remoteErrorConfig)

  where

{-| This module contains the "Knowledge Base" system, which is in essence a unique compromise between data binding
and explicit fetching and writing.

A Knowledge is datum whose state is concretely fuzzy because it must be retrieved from
or synchronized with one or more remote services. The bulk of these functions are intended to be used together as a DSL
that provides very concise reductions and conditionally executed contingencies for bad data, as well as very
succinct mapping from fuzzy knowledge states on to concrete views.

It is also possble to compose transformations and mapping
on to pending remote operation results, so that longer running asynchronous transformations (including fetching and manipulation)
can be composed as deeply as desired. The documentation is underway.

# Definitions
@docs Knowledge, KnowledgeBase, KnowledgeRecord, KnowledgeBaseDelta

# Aliases for Knowledge of a KnowledgeBase or a KnowledgeRecord
@docs BaseKnowledge, RecordKnowledge

# Remote Synchronization
@docs Remote, RemoteMap, RemoteConfig, RemoteMapConfig, QueryTask

# Knowledge constructors
@docs knowledgeOf, forbiddenKnowledge, pendingKnowledge, undecidedKnowledge, unknownKnowledge, voidKnowledge, knowledgeDo

# Knowledge from Existing non-determinant Types
@docs maybeOr, resultOr

# Make Conditional Assumptions about Knowledge
@docs assumeIf, assumeIfNot, assumeIfNow, assumeInCase, assumeInCaseNow, decideBy, doOperation, maybeKnownNow, otherwise

# Transforming Knowledge
@docs therefore, within

# Conditionally Dispatch Operations on Knowledge
@docs dispatchIf, dispatchIfNot, dispatchInCase

# Basic Knowledge Predicates
@docs isUnknown, isNotUnknown, isPending, isNotPending, isUndecided, isNotUndecided, isForbidden, isNotForbidden, isVoid, isNotVoid, isNil, isNotNil, isKnown, isNotKnown, isOperation, isNotOperation

# Integrate Knowledge
@docs knowledgeIntegrate, knowledgeQuery, knowledgeUpdate

# Transforming Knowledge Base Deltas
@docs baseDeltaMap, baseDeltaTherefore

# Knowledge Base Operations
@docs base, baseAt, baseDo, baseErrorHandler, baseIntegrate, baseMember, baseQuery, baseUpdate

# Knowledge Record Operations
@docs record, recordAt, recordBinding, recordContent, recordDo, recordErrorHandler, recordField, recordIntegrate, recordQuery, recordUpdate, reduceNotKnownNowTo

# Configuration
@docs remoteConfig, remoteErrorConfig

-}

import Gigan.Core exposing (..)
import Gigan.Error

import Signal
import Task exposing (Task, andThen, onError)

import Dict exposing (Dict)


{-| -}
type alias Remote v = Task Gigan.Error.Error (Knowledge v)

{-| -}
type alias RemoteMap comparable v = Dict comparable (Remote v)

type alias BaseImpl comparable v = Dict comparable (Knowledge v)
type alias BaseDeltaImpl comparable v = (comparable, Knowledge v)

{-| -}
type alias QueryTask never = Task never ()

{-| A knowledge atom. -}
type Knowledge v =
  Unknown
  | Pending
  -- This is what is done if there appears to be no reason you shouldn't be _allowed_ to look at the
  -- data, but none were found. This will not be encoded in the mirror dictionary (see our wrapper
  -- for ElmFire.Dict from the elmfire-extra package in Elm 0.16) but instead be given as a placeholder
  -- to avoid potentially complex nested maybes or results to deal with uncertainties. This explicitly
  -- tells us in a semantically pure and direct way to tell the user that the data they were looking for
  -- is Void.
  | Void
  -- Gives reason as to why this data are still unknown after a retrieval attempt. If a knowledge
  -- enters in to this state, it's time to intervene, whether automatically or with the aid of user
  -- feedback of some kind.
  | Undecided Gigan.Error.Error
  -- This is what is done if the result from the remote operation for the data is an error explaining why
  -- access to the data was denied.
  | Forbidden Gigan.Error.Error
  -- If a knowledge is an Operation, then any primitives not suffixed with Now
  | Operation (Remote v)
  -- known value of type v.
  | Known v


{-| Configures address to send remote results to, and an error handler for promoting Errors in to Knowledge. The default error handler simply promotes errors to Undecided. -}
type alias RemoteConfig v =
  { address : Signal.Address (Knowledge v)
  , errorHandler : Gigan.Error.Error -> Knowledge v
  }


{-| Configures an address per key to send remote results to, and an error handler per key for promoting Errors in to Knowledge. The default error handler simply promotes errors to Undecided.
The default configuration proxies a single address which accepts a KnowledgeBaseDelta. -}
type alias RemoteMapConfig comparable v =
  { addressOf : comparable -> Signal.Address (Knowledge v)
  , errorHandlerOf : comparable -> Gigan.Error.Error -> Knowledge v
  }


-- Use these for collections, encapsulates many implementation details.
{-| -}
type alias KnowledgeBaseDelta comparable v =
  BaseDeltaImpl comparable v


{-| -}
type alias KnowledgeBase comparable v =
  { base : BaseImpl comparable v
  , deltas : BaseImpl comparable v
  , deltaSink : Signal.Address (BaseDeltaImpl comparable v)
  , config : RemoteMapConfig comparable v
  }


{-| -}
type alias KnowledgeRecord userrecord comparable v =
  { kbase : KnowledgeBase comparable v
  , writes : Dict comparable (Knowledge v -> userrecord -> userrecord)
  , reads : Dict comparable (userrecord -> Knowledge v)
  , record : userrecord
  }


-- Interpret KnowledgeBases and KnowledgeRecords as types of Knowledge. This enables Elm
-- Architecture style nesting, but in the context of dynamic collections of Knowledge.

{-| -}
type alias BaseKnowledge comparable v =
  Knowledge (KnowledgeBase comparable v)

{-| -}
type alias RecordKnowledge userrecord comparable v =
  Knowledge (KnowledgeRecord userrecord comparable v)



{-| Specifies an RemoteConfig with which to close off an remote operation by sending it's results or an
error describing it's failure to the given Signal.Address (Knowledge v). -}
remoteConfig : Signal.Address (Knowledge v) -> RemoteConfig v
remoteConfig address =
  { address = address
  , errorHandler = Undecided -- Default error handler promotes errors to instances of undecided.
  }


{-| Adds an optional special error handler for resolving totally unexpected errors. A "final" error
handler should be provided such that any errors not trapped by a decideBy application still
gracefully recover. By default, a valid knowledge is produced from any error by promoting that
Error to Undecided. -}
remoteErrorConfig : (Gigan.Error.Error -> Knowledge v) -> RemoteConfig v -> RemoteConfig v
remoteErrorConfig handler config =
  { config
  | errorHandler = handler
  }


{-| -}
isUnknown : Knowledge v -> Bool
isUnknown kb =
  case kb of
    Unknown -> True
    _ -> False


{-| -}
isNotUnknown : Knowledge v -> Bool
isNotUnknown = isUnknown >> not


{-| -}
isPending : Knowledge v -> Bool
isPending kb =
  case kb of
    Pending -> True
    _ -> False

{-| -}
isNotPending : Knowledge v -> Bool
isNotPending = isPending >> not


{-| -}
isVoid : Knowledge v -> Bool
isVoid kb =
  case kb of
    Void -> True
    _ -> False


{-| -}
isNotVoid : Knowledge v -> Bool
isNotVoid = isVoid >> not


{-| -}
isNil : Knowledge v -> Bool
isNil kb =
  case kb of
    Unknown -> True
    Void -> True
    _ -> False


{-| -}
isNotNil : Knowledge v -> Bool
isNotNil = isNil >> not


{-| -}
isUndecided : Knowledge v -> Bool
isUndecided kb =
  case kb of
    Undecided _ -> True
    _ -> False


{-| -}
isNotUndecided : Knowledge v -> Bool
isNotUndecided = isUndecided >> not


{-| -}
isForbidden : Knowledge v -> Bool
isForbidden kb =
  case kb of
    Forbidden _ -> True
    _ -> False


{-| -}
isNotForbidden : Knowledge v -> Bool
isNotForbidden = isForbidden >> not


{-| -}
isOperation : Knowledge v -> Bool
isOperation kb =
  case kb of
    Operation _ -> True
    _ -> False


{-| -}
isNotOperation : Knowledge v -> Bool
isNotOperation = isOperation >> not


{-| -}
isKnown : Knowledge v -> Bool
isKnown kb =
  case kb of
    Known _ -> True
    _ -> False


{-| -}
isNotKnown : Knowledge v -> Bool
isNotKnown = isKnown >> not


comprehend : (v -> v') -> Remote v -> Remote v'
comprehend xdcr remote =
  remote `andThen` (therefore xdcr >> Task.succeed)


catchError : (Gigan.Error.Error -> Knowledge v) -> Remote v -> Remote v
catchError decider remote =
  remote `onError` (decider >> Task.succeed)


{-| transform the value itself, if known, producing a knowledge of some new value type value'.
therefores are composed on to the results of remote operations if they represent known knowledge or
further operations to attempt. This allows us to compose async processing stages before knowledge
is finally reduced to a displayed or usable result as deeply and interchangably as we want to,
provided that we always use "therefore" _first_ to lift the knowledge type out before listing
a sequence of simple or contingent reductions. -}
therefore : (v -> v') -> Knowledge v -> Knowledge v'
therefore xdcr kb =
  case kb of
    Unknown -> Unknown
    Pending -> Pending
    Void -> Void

    Undecided err' -> Undecided err'
    Forbidden err' -> Forbidden err'
    Known x' -> Known (xdcr x')

    Operation remote -> Operation (comprehend xdcr remote)


{-| This is for nesting operations on knowledge bases. For example:

    -- this'll write something at the patch foo.bar if "bar" is void.
    baseDo (within <| baseDo (inquireIf isVoid myBarWriter) "bar") "foo" myBase

This code will work on a knowledge base of base knowledges, so that's a nested record. The active
record pattern can be approximated like this, and I've found it extremely handy.

-}
within : (sub -> sub) -> Knowledge sub -> Knowledge sub
within operation ksub =
  case ksub of
    Operation remote -> Operation (comprehend operation remote)
    Known x -> Known (operation x)

    _ -> ksub


-- NOTE : When one applies one of the below primitives to the an instance of Knowledge, the
-- following principal is obeyed: Any primitive which makes sense on an Undecided, Void, or Known
-- Knowledge instance _also applies to future knowledge implied by the existence of an Operation,_
-- such that Remotes can be very cleanly chained in causal order with repeated forward
-- applications of `dispatchIf` and `dispatchInCase`, and reduced with a declared plan just as cleanly
-- at any future stage using `therefore`, `decideBy`, and `assumeIf`. One can freely alternate
-- between Inquiries and causal reductions, whose most important primitives are given in respective
-- lists above, and then pipe the future state of the knowledge back in to any part of the program
-- using the configuration built using `knowledgeSink`, and optionally `resolvingAllBy`.
-- `knowledgeSink` takes an address of type `Signal.Address (Knowledge v)` and produces an
-- `RemoteConfig v`. We can additionally specify an error handler using `resolvingAllBy` that will
-- normalize all Error results in to Knowledge in some uniform way. The default if this is not
-- specified is to promote the offending Error to an Undecided.


{-| Offer a decision on an error classified as 'Undecided'. Undecided errors are the result of some
problem which may or may not be in control of the client. The problem could have been network
partitioning related, or it could have been the result of some user input that was invalid.
The Knowledge state does not encode the manner in which the data was obtained, so that that
concern can be separated, since it would otherwise greatly complicate the knowledge primitive.
At this point, it should be apparent that Knowledge is the state in a state machine composed by
sequential transformations of a Knowledge by these pipeline functions. The goal of this is clean
implicit causality and control flow in complex declarations about how to handle non-determinant
state answered by a finnicky outside oracle that may or may not make sense or be prompt. -}
decideBy : (Gigan.Error.Error -> Knowledge v) -> Knowledge v -> Knowledge v
decideBy decider kb =
  case kb of
    Undecided err' -> decider err'
    Operation remote -> Operation (catchError decider remote)

    _ -> kb


{-| If some predicate `satisfies` is satisfied by the knowledge `kb`, then we make the following
assumption. This also applies to future knowledge implied by the existence of an Operation. -}
assumeIf : (Knowledge v -> Bool) -> v -> Knowledge v -> Knowledge v
assumeIf satisfies assume kb =
  case kb of
    Operation remote ->
      Operation (remote `andThen` (assumeIf satisfies assume >> Task.succeed))

    _ ->
      if satisfies kb then therefore (always assume) kb else kb


{-| Negation of assumeIf -}
assumeIfNot : (Knowledge v -> Bool) -> v -> Knowledge v -> Knowledge v
assumeIfNot satisfies assume kb =
  assumeIf (satisfies >> not) assume kb


{-| If `possibleAssumption` yields some value `value'` when a Knowledge is applied, then
that value is used to overwrite the knowledge with an assumption `Known value'`, otherwise the
Knowledge is unaffected. -}
assumeInCase : (Knowledge v -> Maybe v) -> Knowledge v -> Knowledge v
assumeInCase possibleAssumption kb =
  case kb of
    Operation remote ->
      Operation (remote `andThen` (assumeInCase possibleAssumption >> Task.succeed))

    _ ->
      Maybe.map Known (possibleAssumption kb)
      |> Maybe.withDefault kb


{-| If some predicate `satisfies` is satisfied by the knowledge `kb`, then we make the following
remote operation. -}
dispatchIf : (Knowledge v -> Bool) -> Remote v -> Knowledge v -> Knowledge v
dispatchIf satisfies remote kb =
  case kb of
    Operation remote ->
      Operation (remote `andThen` (dispatchIf satisfies remote >> Task.succeed))

    _ ->
      dispatchInCase (if satisfies kb then always (Just remote) else always Nothing) kb


{-| Negation of dispatchIf -}
dispatchIfNot : (Knowledge v -> Bool) -> Remote v -> Knowledge v -> Knowledge v
dispatchIfNot satisfies remote kb =
  dispatchIf (satisfies >> not) remote kb


{-| The root remote operation transformation which maybe produces a remote task. If it does, then that becomes
an Operation, otherwise the Knowledge is unaffected. -}
dispatchInCase : (Knowledge v -> Maybe (Remote v)) -> Knowledge v -> Knowledge v
dispatchInCase possibleOperation kb =
  case kb of
    Operation remote ->
      Operation (remote `andThen` (dispatchInCase possibleOperation >> Task.succeed))

    _ ->
      Maybe.map doOperation (possibleOperation kb)
      |> Maybe.withDefault kb


-- NOTE : These primitives force a reduction now even for an remote operation type
-- convenient conversion to a maybe after all mappings are given. This is intended for use when
-- mapping the state of the content to an actual display.

{-| If a knowledge is known, then give Just it's value, otherwise Nothing. -}
maybeKnownNow : Knowledge v' -> Maybe v'
maybeKnownNow kb' =
  case kb' of
    Known x' -> Just x'
    _ -> Nothing

{-| If the predicate is satisfied, replace the knowledge with some known value. -}
assumeIfNow : (Knowledge v' -> Bool) -> v' -> Knowledge v' -> Knowledge v'
assumeIfNow satisfies assumption kb' =
  if satisfies kb' then Known assumption else kb'


{-| -}
assumeInCaseNow : (Knowledge v' -> Maybe v') -> Knowledge v' -> Knowledge v'
assumeInCaseNow possibleAssumption kb' =
  Maybe.map Known (possibleAssumption kb')
  |> Maybe.withDefault kb'


{-| This is the special reduction we use to collapse away the Knowledge type, determining a final
value to work with. -}
reduceNotKnownNowTo : v' -> Knowledge v' -> v'
reduceNotKnownNowTo assumption kb' =
  case kb' of
    Known x' -> x'
    _ -> assumption


{-| Preferred shorthand for reduceNotKnownNowTo -}
otherwise : v' -> Knowledge v' -> v'
otherwise = reduceNotKnownNowTo


{-| -}
unknownKnowledge : Knowledge v
unknownKnowledge = Unknown


{-| -}
pendingKnowledge : Knowledge v
pendingKnowledge = Pending


{-| -}
voidKnowledge : Knowledge v
voidKnowledge = Void


{-| -}
undecidedKnowledge : Gigan.Error.Error -> Knowledge v
undecidedKnowledge = Undecided


{-| -}
forbiddenKnowledge : Gigan.Error.Error -> Knowledge v
forbiddenKnowledge = Forbidden


{-| -}
knowledgeOf : v -> Knowledge v
knowledgeOf = Known


{-| -}
resultOr : (Gigan.Error.Error -> Knowledge v) -> Result Gigan.Error.Error v -> Knowledge v
resultOr errorKnowledge result =
  case result of
    Result.Ok data -> knowledgeOf data
    Result.Err err' -> errorKnowledge err'


{-| -}
maybeOr : Knowledge v -> Maybe v -> Knowledge v
maybeOr nothingKnowledge maybeValue =
  Maybe.map Known maybeValue
  |> Maybe.withDefault nothingKnowledge


{-| -}
doOperation : Remote v -> Knowledge v
doOperation = Operation


{-| -}
knowledgeDo : (Knowledge v -> Knowledge v) -> comparable -> Knowledge v -> Knowledge v
knowledgeDo transform _ kb =
  transform kb


{-| -}
knowledgeUpdate : KnowledgeBaseDelta comparable v -> Knowledge v -> Knowledge v
knowledgeUpdate (_, kb') kb = kb'


{-| -}
knowledgeQuery : RemoteConfig v -> Knowledge v -> Maybe (QueryTask never)
knowledgeQuery config kb =
  Maybe.map (declareRemoteResultDispatch_ config) (maybeRemoteTask_ kb)


{-| -}
knowledgeIntegrate : RemoteConfig v -> Knowledge v -> Knowledge v
knowledgeIntegrate config kb =
  if isOperation kb then Pending else kb


{-- KNOWLEDGE BASE --}

-- Knowledge base is configured with an address to send deltas to.
-- When one requires contacting the outside information source, an Operation,
-- one possibly gets a task for each recompute of the program where that task dispatches the
-- fetch or compute operations given as remote operations. Any Operation Knowledge items produce their
-- respective tasks, which send their results to the KnowledgeBase's address. These tasks are
-- always dispatched in parallel by folding the task list by spawn ... andThen.

{-| -}
base : Signal.Address (KnowledgeBaseDelta comparable v) -> KnowledgeBase comparable v
base address =
  { base = Dict.empty
  , deltas = Dict.empty
  , deltaSink = address
  , config = baseRemoteConfig_ address Undecided
  }


{-| -}
baseErrorHandler : (Gigan.Error.Error -> Knowledge v) -> KnowledgeBase comparable v -> KnowledgeBase comparable v
baseErrorHandler errorHandler kbase =
  { kbase
  | config = baseRemoteConfig_ kbase.deltaSink errorHandler
  }


{-| -}
baseAt : comparable -> KnowledgeBase comparable v -> Knowledge v
baseAt key kbase =
  if baseMember_ key kbase.deltas then
    baseAt_ key kbase.deltas
  else
    baseAt_ key kbase.base


{-| -}
baseMember : comparable -> KnowledgeBase comparable v -> Bool
baseMember key = baseAt key >> (/=) Unknown


{-| -}
baseDo : (Knowledge v -> Knowledge v) -> comparable -> KnowledgeBase comparable v -> KnowledgeBase comparable v
baseDo transform key kbase =
  let
    (_, kb') =
      if baseMember_ key kbase.deltas then
        baseDeltaTransformAt_ transform key kbase.deltas
      else
        baseDeltaTransformAt_ transform key kbase.base
  in
    { kbase
    | deltas = Dict.insert key kb' kbase.deltas
    }


{-| -}
baseDeltaTherefore : (v -> v') -> KnowledgeBaseDelta comparable v -> KnowledgeBaseDelta comparable v'
baseDeltaTherefore xdcr (key, kb') =
  (key, therefore xdcr kb')


{-| -}
baseDeltaMap : (Knowledge v -> Knowledge v) -> KnowledgeBaseDelta comparable v -> KnowledgeBaseDelta comparable v
baseDeltaMap transform (key, kb') =
  (key, transform kb')


{-| -}
baseUpdate : KnowledgeBaseDelta comparable v -> KnowledgeBase comparable v -> KnowledgeBase comparable v
baseUpdate (key, kb') kbase =
  { kbase
  | deltas = Dict.insert key kb' kbase.deltas
  }


{-| -}
baseQuery : KnowledgeBase comparable v -> Maybe (QueryTask never)
baseQuery kbase =
  baseDeltaDictQuery_ kbase.config kbase.deltas


{-| -}
baseIntegrate : KnowledgeBase comparable v -> KnowledgeBase comparable v
baseIntegrate kbase =
  { kbase
  | base = baseDeltaDictIntegrate_ kbase.deltas kbase.base
  , deltas = Dict.empty
  }


{-- KNOWLEDGE RECORD --}


{-| -}
record : Signal.Address (KnowledgeBaseDelta comparable v) -> userrecord -> KnowledgeRecord userrecord comparable v
record address rec =
  { kbase = base address
  , writes = Dict.empty
  , reads = Dict.empty
  , record = rec
  }


{-| -}
recordField : (Knowledge v -> userrecord -> userrecord) -> (userrecord -> Knowledge v) -> comparable -> KnowledgeRecord userrecord comparable v -> KnowledgeRecord userrecord comparable v
recordField write' read' key krecord =
  { krecord
  | writes = Dict.insert key write' krecord.writes
  , reads = Dict.insert key read' krecord.reads
  }


{-| -}
recordErrorHandler : (Gigan.Error.Error -> Knowledge v) -> KnowledgeRecord userrecord comparable v -> KnowledgeRecord userrecord comparable v
recordErrorHandler errorHandler krecord =
  { krecord
  | kbase = baseErrorHandler errorHandler krecord.kbase
  }


{-| -}
recordContent : KnowledgeRecord userrecord comparable v -> userrecord
recordContent = .record


{-| -}
recordBinding : KnowledgeRecord userrecord comparable v -> KnowledgeBase comparable v
recordBinding = .kbase


{-| -}
recordAt : (userrecord -> Knowledge v) -> KnowledgeRecord userrecord comparable v -> Knowledge v
recordAt getter = .record >> getter


{-| -}
recordUpdate : KnowledgeBaseDelta comparable v -> KnowledgeRecord userrecord comparable v -> KnowledgeRecord userrecord comparable v
recordUpdate kbdelta krecord =
  recordWrite_ kbdelta krecord


{-| -}
recordDo : (Knowledge v -> Knowledge v) -> comparable -> KnowledgeRecord userrecord comparable v -> KnowledgeRecord userrecord comparable v
recordDo transform key krecord =
  -- easy as pie! :`-D
  recordRead_ key krecord
  |> transform
  |> \kb' -> recordWrite_ (key, kb') krecord


{-| -}
recordQuery : KnowledgeRecord userrecord comparable v -> Maybe (QueryTask never)
recordQuery =
  .kbase >> baseQuery


{-| -}
recordIntegrate : KnowledgeRecord userrecord comparable v -> KnowledgeRecord userrecord comparable v
recordIntegrate krecord =
  { krecord | kbase = baseIntegrate krecord.kbase }


-- NOTE : Record fields are accessed by name by the user (supplement Errata, please)

-- NOTE : The following is the INTERNAL implementation of KnowledgeBase. Use KnowledgeBase and it's
-- functions (beginning with base, no underscore at the end) to manipulate knowledge bases.


recordRWCrashMessage_ : comparable -> String
recordRWCrashMessage_ key =
  "For KnowledgeRecord " ++ (toString key) ++ " "
  ++ "there is not a complete read/write pair, which violates the constraints of the "
  ++ "system! ONLY USE THE PROVIDED PRIMITIVES TO MANIPULATE KnowledgeRecords, it is an "
  ++ "opaque type."


recordRW_ : comparable -> KnowledgeRecord userrecord comparable v -> Maybe { write : Knowledge v -> userrecord -> userrecord, read : userrecord -> Knowledge v }
recordRW_ key krecord =
  let
    arrangement write' read' =
      Just { write = write', read = read' }

  in
    Dict.get key krecord.writes
    |> \maybeWrite -> Dict.get key krecord.reads
    |> \maybeRead ->
      case (maybeWrite, maybeRead) of
        (Just write', Just read') -> Just { read = read', write = write' }
        (Nothing, Nothing) ->
          Debug.log "For KnowledgeRecord key" key
          |> \_ -> Debug.log "Expecting to find read and write functions but found" Nothing

        (Just _, Nothing) -> Debug.crash (recordRWCrashMessage_ key)
        (Nothing, Just _) -> Debug.crash (recordRWCrashMessage_ key)


recordWrite_ : KnowledgeBaseDelta comparable v -> KnowledgeRecord userrecord comparable v -> KnowledgeRecord userrecord comparable v
recordWrite_ (key, kb') krecord =
  recordRW_ key krecord
  |> Maybe.map (\{write} ->
    { krecord
    | kbase = baseUpdate (key, kb') krecord.kbase
    , record = write kb' krecord.record
    })
  |> Maybe.withDefault krecord


recordRead_ : comparable -> KnowledgeRecord userrecord comparable v -> Knowledge v
recordRead_ key krecord =
  recordRW_ key krecord
  |> Maybe.map (\{read} -> read krecord.record)
  |> Maybe.withDefault Unknown


baseAt_ : comparable -> BaseImpl comparable v -> Knowledge v
baseAt_ key kbdict =
  Dict.get key kbdict
  |> Maybe.withDefault Unknown


baseMember_ : comparable -> BaseImpl comparable v -> Bool
baseMember_ key kbdict =
  case baseAt_ key kbdict of
    Unknown -> False
    _ -> True


maybeRemoteTask_ : Knowledge v -> Maybe (Remote v)
maybeRemoteTask_ kb =
  case kb of
    Operation remote -> Just remote
    _ -> Nothing


declareRemoteResultDispatch_ : RemoteConfig v -> Remote v -> QueryTask never
declareRemoteResultDispatch_ config remote =
  catchError config.errorHandler remote
    `andThen` (Signal.send config.address) -- got a new Knowledge as a result of the Operation
    `onError` (Undecided >> Signal.send config.address) -- last ditch if the error handler erred.


remoteConfigAt_ : comparable -> RemoteMapConfig comparable v -> RemoteConfig v
remoteConfigAt_ key configMap =
  { address = configMap.addressOf key
  , errorHandler = configMap.errorHandlerOf key
  }


baseDeltaDictRemoteMap_ : RemoteMapConfig comparable v -> BaseImpl comparable v -> RemoteMap comparable v
baseDeltaDictRemoteMap_ configMap deltas =
  Dict.foldl
    (\key kb' resultMap -> maybeRemoteTask_ kb'
    |> Maybe.map (flip (Dict.insert key) resultMap) -- with task added
    |> Maybe.withDefault resultMap) -- unchanged
    Dict.empty
    deltas


baseDeltaIntegrate_ : BaseDeltaImpl comparable v -> BaseImpl comparable v -> BaseImpl comparable v
baseDeltaIntegrate_ (key, kb') kbdict0 =
  case kb' of
    Unknown   -> Dict.remove key kbdict0         -- Unknowns are removed.
    Operation _ -> Dict.insert key Pending kbdict0 -- Operation becomes pending.
    _         -> Dict.insert key kb' kbdict0     -- Anything else is updated.


baseDeltaDictIntegrate_ : BaseImpl comparable v -> BaseImpl comparable v -> BaseImpl comparable v
baseDeltaDictIntegrate_ deltas kbdict0 =
  Dict.foldl (curry baseDeltaIntegrate_) kbdict0 deltas


-- `baseTransformAt` can be combined by currying easily with any of the above knowledge translations
-- ending in (Knowledge v -> Knowledge v)
baseDeltaTransformAt_ : (Knowledge v -> Knowledge v) -> comparable -> BaseImpl comparable v -> BaseDeltaImpl comparable v
baseDeltaTransformAt_ transform key kbdict =
  Dict.get key kbdict
  |> Maybe.map transform
  |> Maybe.withDefault (transform Unknown)
  |> (,) key


remoteMapQuery_ : RemoteMapConfig comparable v -> RemoteMap comparable v -> Maybe (QueryTask never)
remoteMapQuery_ configMap remoteMap =
  let
    declareDispatch key remote =
      declareRemoteResultDispatch_ (remoteConfigAt_ key configMap) remote

    foldOperation key remote mtask =
      case mtask of
        Just task' -> Just (Task.spawn task' `andThen` \_ -> declareDispatch key remote)
        Nothing -> Just (declareDispatch key remote)
  in
    Dict.foldl
      foldOperation
      Nothing
      remoteMap


baseDeltaDictQuery_ : RemoteMapConfig comparable v -> BaseImpl comparable v -> Maybe (QueryTask never)
baseDeltaDictQuery_ configMap deltas =
  baseDeltaDictRemoteMap_ configMap deltas
  |> remoteMapQuery_ configMap



baseRemoteConfig_ : Signal.Address (BaseDeltaImpl comparable v) -> (Gigan.Error.Error -> Knowledge v) -> RemoteMapConfig comparable v
baseRemoteConfig_ address errorHandler =
  { addressOf = (\key -> Signal.forwardTo address (\kb' -> (key, kb')))
  , errorHandlerOf = always errorHandler
  }
