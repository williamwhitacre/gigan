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


module Gigan.Core

  (AgentStatus, ComputedResult, ComputedSuccess, FeedbackMethod, OrbiterInput,
  OrbiterOutput, OrbiterSnapshot, OrbiterTap, OrbiterTask, TaskDispatchment,
  UpdatedModel, ViewOutput,

  defProgram, defProgram',

  orbits, orbitsWithWork, withInputs, withLazySequenceInputs, withSequenceInputs, sieve, (+-->),

  updated, presented, withTasks, withDispatchment, withChildren, viewOutputTask,

  actionTask, errorTask, computeTask, computedSuccess, noActions, nilTask,

  successTap, errorTap, atomicSuccessTap, asyncSuccessTap, it'sErrorTap, itself, itselfDeferred,
  thisAddress, thisAddressDeferred, thisCustomAddress, thisCustomAddressDeferred, thisErrorTap,
  thisForwardTap, thisTap,

  combineDispatchments, collapseTasks, dispatchTasks, dispatchmentHasWork, dispatchmentTask,
  promoteDispatchment,

  orbiterSnapshot, orbiterSnapshotAddDispatchment, orbiterSnapshotDispatch,
  orbiterSnapshotPresent, orbiterSnapshotStage, orbiterSnapshotUpdate, performCycle,

  orbiterAgent, orbiterSuccessAgent, orbiterFailureAgent, orbiterBinaryAgent, orbiterBlindAgent,
  orbiterNilAgent, orbiterResultAgent, ignoreError)

  where

{-| This module is the main application scaffolding. Detailed documentation in progress.

# Definitions
@docs AgentStatus, ComputedResult, ComputedSuccess, FeedbackMethod, OrbiterInput, OrbiterOutput, OrbiterSnapshot, OrbiterTap, OrbiterTask, TaskDispatchment, UpdatedModel, ViewOutput

# Define Orbiter Programs
@docs defProgram, defProgram'

# Run Orbiter Programs
@docs orbits, orbitsWithWork, withInputs, withLazySequenceInputs, withSequenceInputs, sieve, (+-->)

# UpdatedModel and ViewOutput Manipulation
@docs updated, presented, withTasks, withDispatchment, withChildren, viewOutputTask

# Dispatch Actions and Errors
@docs actionTask, errorTask, computeTask, computedSuccess, noActions, nilTask

# Intercept and Route Action and Error Outputs
@docs successTap, errorTap, atomicSuccessTap, asyncSuccessTap, it'sErrorTap, itself, itselfDeferred, thisAddress, thisAddressDeferred, thisCustomAddress, thisCustomAddressDeferred, thisErrorTap, thisForwardTap, thisTap

# Handling Tasks and TaskDispatchment
@docs combineDispatchments, collapseTasks, dispatchTasks, dispatchmentHasWork, dispatchmentTask, promoteDispatchment

# Manipulate Orbiter Snapshots
@docs orbiterSnapshot, orbiterSnapshotAddDispatchment, orbiterSnapshotDispatch, orbiterSnapshotPresent, orbiterSnapshotStage, orbiterSnapshotUpdate, performCycle

# Orbiter Task Agents
@docs orbiterAgent, orbiterSuccessAgent, orbiterFailureAgent, orbiterBinaryAgent, orbiterBlindAgent, orbiterNilAgent, orbiterResultAgent, ignoreError

-}

import Signal
import Signal.Extra exposing ((<~), (~>), (~))
import Debug


import Result


import Task exposing (Task, andThen, onError)
import Task.Extra


import Lazy.List exposing (LazyList, (:::), (+++))
import Time exposing (Time)


{-| todo doc -}
type FeedbackMethod =
  Atomically | Asynchronously


{-| todo doc -}
type alias OrbiterTask bad a = Task bad (ComputedSuccess a)


{-| todo doc -}
type alias ComputedResult bad a = Result bad (ComputedSuccess a)


{-| todo doc -}
type alias ComputedSuccess a =
  { sequence : List a
  , method : FeedbackMethod
  }


{-| todo doc -}
type alias OrbiterInput a b c bad =
  { inputs  : LazyList (Signal (LazyList a)) -- lazy list used here to reduce aggregation time
  , model0  : b
  , present : Signal.Address (List a) -> Time -> b -> ViewOutput a c bad
  , stage   : Signal.Address (List a) -> Time -> b -> UpdatedModel a b bad
  , update  : a -> Time -> b -> UpdatedModel a b bad
  }


{-| todo doc -}
type alias OrbiterSnapshot a b bad =
  { model' : b
  , dispatchment : TaskDispatchment bad a
  }


{-| todo doc -}
type alias OrbiterOutput a b c bad =
  { view' : Signal c
  , model' : Signal b
  , now : Signal Time
  , actions : Signal (List a) -- as a matter of robustness, we should forcibly reduce actions
                              -- action to normal lists so that no spacetime leaks occur on
                              -- account of unbounded laziness.
  , tasks : Signal (OrbiterTask bad a)
  , address : Signal.Address (List a)
  , lazyAddress : Signal.Address (LazyList a) -- a performance boost where you want it,
                                              -- simplicity where it's unneeded
  }


{-| todo doc -}
type alias TaskDispatchment bad a =
  { taskExec : LazyList (OrbiterTask bad a)
  }


{-| todo doc -}
type alias UpdatedModel a b bad =
  { dispatchment : TaskDispatchment bad a
  , model' : b
  }


{-| todo doc -}
type alias ViewOutput a c bad =
  { dispatchment : TaskDispatchment bad a
  , view' : c
  }


{-| todo doc -}
type alias OrbiterTap bad a =
  (Signal.Address (List a) -> OrbiterTask bad a -> OrbiterTask bad a)


{-| todo doc -}
noActions : List a
noActions = []


{-| todo doc -}
computedSuccess : List a -> ComputedSuccess a
computedSuccess actions =
  { sequence = actions, method = Atomically }


{-| todo doc -}
nilTask : OrbiterTask bad a
nilTask = actionTask noActions


{-| todo doc -}
actionTask : List a -> OrbiterTask bad a
actionTask actions = Task.succeed (computedSuccess actions)


{-| todo doc -}
errorTask : bad -> OrbiterTask bad a
errorTask error' = Task.fail (error')


{-butBeforeActionsDo : Task y x -> OrbiterTask bad a -> OrbiterTask bad a
butBeforeActionsDo task orbiterTask =
  orbiterTask `andThen` \a0 -> task `andThen` \_ -> Task.succeed a0-}


{-| todo doc -}
computeTask : (data -> ComputedResult bad a) -> data -> OrbiterTask bad a
computeTask compute' data =
  let
    computation data =
      case compute' data of
        Result.Ok result -> Task.succeed result
        Result.Err err -> Task.fail err

  in
    (Task.succeed ()) `andThen` (\_ -> computation data)


{-| todo doc -}
atomicSuccessTap : Signal.Address (List a) -> OrbiterTask bad a -> OrbiterTask bad a
atomicSuccessTap address task =
  let
    extractedSequence = task `andThen` (\x -> Task.succeed x.sequence)

  in
    (Task.Extra.interceptSuccess address extractedSequence)
      `andThen` actionTask


{-| todo doc -}
asyncSuccessTap : Signal.Address (List a) -> OrbiterTask bad a -> OrbiterTask bad a
asyncSuccessTap address task =
  let
    fitting act prior =
      prior `andThen` \_ -> atomicSuccessTap address <| actionTask [act]


    spread sequenceDispatcher =
      List.foldl fitting nilTask sequenceDispatcher.sequence
  in
    task `andThen` spread
    |> Task.Extra.interceptSuccess (Signal.forwardTo address .sequence)



{-| todo doc -}
successTap : Signal.Address (List a) -> OrbiterTask bad a -> OrbiterTask bad a
successTap address task =
  let
    attachFitting result =
      Task.succeed result
      |> case result.method of
            Atomically -> atomicSuccessTap address
            Asynchronously -> asyncSuccessTap address

  in
    task `andThen` attachFitting


{-| todo doc -}
errorTap : Signal.Address (List a) -> (bad -> List a) -> OrbiterTask bad a -> OrbiterTask bad a
errorTap address errorActionTransform task =
  Signal.forwardTo address errorActionTransform
  |> flip Task.Extra.interceptError task


{-| todo doc -}
ignoreError : bad -> List a
ignoreError = always noActions


{-| todo doc -}
defProgram
  :  (Signal.Address (List a) -> Time -> b -> ViewOutput a c bad)
  -> (a -> Time -> b -> UpdatedModel a b bad)
  -> b
  -> OrbiterInput a b c bad
defProgram present update model = defProgram' present (\_ _ m' -> updated m') update model


{-| todo doc -}
defProgram'
  :  (Signal.Address (List a) -> Time -> b -> ViewOutput a c bad)
  -> (Signal.Address (List a) -> Time -> b -> UpdatedModel a b bad)
  -> (a -> Time -> b -> UpdatedModel a b bad)
  -> b
  -> OrbiterInput a b c bad
defProgram' present stage update model =
  { inputs = Lazy.List.empty
  , model0 = model
  , present = present
  , update = update
  , stage = stage
  }



{-
  e.g. defProgram present update model0 `withSequenceInputs` myInputs
-}

{-| todo doc -}
withInputs : OrbiterInput a b c bad -> List (Signal a) -> OrbiterInput a b c bad
withInputs inR sigs =
  { inR
  | inputs = inR.inputs +++ (List.foldr (\x ls -> (x ~> Lazy.List.singleton) ::: ls) Lazy.List.empty sigs)
  }


{-| todo doc -}
withSequenceInputs : OrbiterInput a b c bad -> List (Signal (List a)) -> OrbiterInput a b c bad
withSequenceInputs inR sigs =
  { inR
  | inputs = inR.inputs +++ (List.foldr (\x ls -> (x ~> Lazy.List.fromList) ::: ls) Lazy.List.empty sigs)
  }


-- An alternative lazy list address for cases where you really need lazy lists for the performance
-- boost.

{-| todo doc -}
withLazySequenceInputs : OrbiterInput a b c bad -> List (Signal (LazyList a)) -> OrbiterInput a b c bad
withLazySequenceInputs inR sigs =
  { inR
  | inputs = inR.inputs +++ (List.foldr (\x ls -> x ::: ls) Lazy.List.empty sigs)
  }


{-| todo doc -}
dispatchTasks : List (OrbiterTask bad a) -> TaskDispatchment bad a
dispatchTasks = Lazy.List.fromList >> dispatchment_


{-| todo doc -}
dispatchmentTask : TaskDispatchment bad a -> OrbiterTask bad a
dispatchmentTask = reduceDispatchment_


{-| todo doc -}
dispatchmentHasWork : TaskDispatchment bad a -> Bool
dispatchmentHasWork dispatchment =
  (Lazy.List.length dispatchment.taskExec) > 0


{-| todo doc -}
viewOutputTask : ViewOutput a c bad -> OrbiterTask bad a
viewOutputTask output = output.dispatchment |> dispatchmentTask


{-| todo doc -}
combineDispatchments : TaskDispatchment bad a -> TaskDispatchment bad a -> TaskDispatchment bad a
combineDispatchments dsp dsp' =
  { dsp | taskExec = dsp.taskExec +++ dsp'.taskExec }


{-| todo doc -}
promoteDispatchment : (List a -> List a') -> TaskDispatchment bad a -> TaskDispatchment bad a'
promoteDispatchment xdcr dsp =
  { dsp
  | taskExec =
    Lazy.List.map
      (\task -> task `andThen` \a0 -> actionTask (xdcr a0.sequence))
      dsp.taskExec
  }


{-| todo doc -}
withTasks
  :  List (OrbiterTask bad a)
  -> { anything | dispatchment : TaskDispatchment bad a }
  -> { anything | dispatchment : TaskDispatchment bad a }
withTasks tasks out' =
  { out'
  | dispatchment = dispatch_ out'.dispatchment tasks
  }


{-| todo doc -}
withDispatchment
  : TaskDispatchment bad a
  -> { anything | dispatchment : TaskDispatchment bad a }
  -> { anything | dispatchment : TaskDispatchment bad a }
withDispatchment dispatchment out' =
  { out'
  | dispatchment = combineDispatchments out'.dispatchment dispatchment
  }


{-| todo doc -}
withChildren
  : List { anything | dispatchment : TaskDispatchment bad a }
  -> { anything | dispatchment : TaskDispatchment bad a }
  -> { anything | dispatchment : TaskDispatchment bad a }
withChildren children out' =
  { out'
  | dispatchment = List.foldl
      (\ch dsp -> combineDispatchments dsp ch.dispatchment)
      out'.dispatchment
      children
  }


-- ORBITER AGENTS
--
--  These primitives are designed to make it more concise to specify complex branching contingencies
-- in tasks before closing them off either with error reports (see "taps", such as it'sErrorTap).
-- or with lists of actions to execute on success. We allow control over whether or not something is
-- interpreted as success or failure at a level that enables total intervention, so business logic
-- complexity is not limited when constructing contingency graphs (task branching). Using the Elm
-- Architecture as per usual, we can have logarithmic code size.
--

{-| todo doc -}
type AgentStatus bad a =
  AgentSuccess (List a)
  | AgentFailure bad


{-| todo doc -}
agentStatusResult status =
  case status of
    AgentSuccess ls -> Task.succeed (Result.Ok (computedSuccess ls))
    AgentFailure err' -> Task.succeed (Result.Err err')


{-| todo doc -}
orbiterAgent : (x -> AgentStatus bad a) -> (y -> AgentStatus bad a) -> Task y x -> OrbiterTask bad a
orbiterAgent onSuccess onFailure task =
  (task
    `andThen` (onSuccess >> agentStatusResult)
    `onError` (onFailure >> agentStatusResult))
    `andThen`
      \result ->
        case result of
          Result.Ok computedSuccess -> Task.succeed computedSuccess
          Result.Err problem -> Task.fail problem



{-| todo doc -}
orbiterBinaryAgent : AgentStatus bad a -> AgentStatus bad a -> Task y x -> OrbiterTask bad a
orbiterBinaryAgent onSuccessResult onFailureResult =
  orbiterAgent (always onSuccessResult) (always onFailureResult)

{-| todo doc -}
orbiterSuccessAgent : (x -> AgentStatus bad a) -> AgentStatus bad a -> Task y x -> OrbiterTask bad a
orbiterSuccessAgent onSuccess onFailureResult =
  orbiterAgent onSuccess (always onFailureResult)

{-| todo doc -}
orbiterFailureAgent : AgentStatus bad a -> (y -> AgentStatus bad a) -> Task y x -> OrbiterTask bad a
orbiterFailureAgent onSuccessResult onFailure =
  orbiterAgent (always onSuccessResult) onFailure

{-| todo doc -}
orbiterResultAgent : (Result y x -> AgentStatus bad a) -> Task y x -> OrbiterTask bad a
orbiterResultAgent produceResult =
  orbiterAgent (Result.Ok >> produceResult) (Result.Err >> produceResult)

{-| todo doc -}
orbiterBlindAgent : AgentStatus bad a -> Task y x -> OrbiterTask bad a
orbiterBlindAgent result =
  orbiterBinaryAgent result result

{-| todo doc -}
orbiterNilAgent : Task y x -> OrbiterTask bad a
orbiterNilAgent =
  orbiterBlindAgent (AgentSuccess [])



dispatchment_
  :  LazyList (OrbiterTask bad a)
  -> TaskDispatchment bad a
dispatchment_ taskExec =
  { taskExec = taskExec
  }


dispatch_
  :  TaskDispatchment bad a
  -> List (OrbiterTask bad a)
  -> TaskDispatchment bad a
dispatch_ dispatchment seq =
  { dispatchment
  | taskExec = dispatchment.taskExec +++ (Lazy.List.fromList seq)
  }


reduceDispatchment_ : TaskDispatchment bad a -> OrbiterTask bad a
reduceDispatchment_ dispatchmentRecord =
  Lazy.List.foldl
    (\task task' ->
      task
        `andThen` \a0 -> task'
        `andThen` \a1 -> actionTask (List.append a0.sequence a1.sequence)
    )
    (actionTask [])
    dispatchmentRecord.taskExec



-- TEARS OF :JOY:

{-| todo doc -}
sieve : OrbiterOutput a b c bad -> Signal (Task z ())
sieve q'' =
  q''.tasks ~> \task' -> Debug.log "TASK RCV" task'
    `andThen` (\final -> Debug.log "TASK OK" final |> \_ -> Task.succeed ())
    `onError` (\err -> Debug.log "ERROR" err |> \_ -> Task.succeed ())



{--
  give an updated model with a list of task lists to execute sequentially
--}

{-| todo doc -}
updated : b -> UpdatedModel a b bad
updated model =
  { model' = model
  , dispatchment = dispatchment_ Lazy.List.empty
  }


{-| todo doc -}
presented : c -> ViewOutput a c bad
presented view =
  { view' = view
  , dispatchment = dispatchment_ Lazy.List.empty
  }


{-| todo doc -}
orbits : OrbiterInput a b c bad -> OrbiterOutput a b c bad
orbits defs =
  defs `orbitsWithWork` nilTask


{-| todo doc -}
orbiterSnapshot : b -> TaskDispatchment bad a -> OrbiterSnapshot a b bad
orbiterSnapshot model0 dispatchment =
  { dispatchment = dispatchment
  , model' = model0 }


{-| todo doc -}
orbiterSnapshotAddDispatchment : TaskDispatchment bad a -> OrbiterSnapshot a b bad -> OrbiterSnapshot a b bad
orbiterSnapshotAddDispatchment dispatchment' { dispatchment, model' } =
  { dispatchment = combineDispatchments dispatchment dispatchment'
  , model' = model'
  }


{-| todo doc -}
orbiterSnapshotUpdate
  : { k | update : a -> Time -> b -> UpdatedModel a b bad }
  -> List a
  -> Time
  -> OrbiterSnapshot a b bad
  -> OrbiterSnapshot a b bad
orbiterSnapshotUpdate { update } actions now state =
  let
    applyAction action state =
      update action now state.model'
      |> \updated' -> orbiterSnapshotAddDispatchment
        updated'.dispatchment
        { state | model' = updated'.model' }

  in
    List.foldl applyAction state actions


{-
  , present : (Signal.Address (List a) -> Time -> b -> ViewOutput a c bad)
  , stage : (Signal.Address (List a) -> Time -> b -> UpdatedModel a b bad)
  , update : (a -> Time -> b -> UpdatedModel a b bad)
-}

{-| todo doc -}
orbiterSnapshotStage
  : { k | stage : Signal.Address (List a) -> Time -> b -> UpdatedModel a b bad }
  -> Signal.Address (List a)
  -> Time
  -> OrbiterSnapshot a b bad
  -> OrbiterSnapshot a b bad
orbiterSnapshotStage { stage } address now state =
  stage address now state.model'
  |> \{dispatchment, model'} ->
    orbiterSnapshotAddDispatchment dispatchment { state | model' = model' }


{-| todo doc -}
orbiterSnapshotPresent
  : { k | present : Signal.Address (List a) -> Time -> b -> ViewOutput a c bad }
  -> Signal.Address (List a)
  -> Time
  -> OrbiterSnapshot a b bad
  -> ViewOutput a c bad
orbiterSnapshotPresent { present } address now state =
  present address now state.model'


{-| todo doc -}
orbiterSnapshotDispatch : OrbiterSnapshot a b bad -> (OrbiterSnapshot a b bad, TaskDispatchment bad a)
orbiterSnapshotDispatch state =
  (,)
    { state
    | dispatchment = dispatchTasks []
    }
    state.dispatchment


{-| todo doc -}
collapseTasks : OrbiterTask bad a -> OrbiterTask bad a -> OrbiterTask bad a
collapseTasks task task' =
  task `andThen` \a0 -> task' `andThen` \a1 -> actionTask (List.append a0.sequence a1.sequence)


{-| todo doc -}
performCycle
  : OrbiterInput a b c bad
  -> Signal.Address (List a)
  -> (Time, List a)
  -> (OrbiterSnapshot a b bad, TaskDispatchment bad a)
  -> (OrbiterSnapshot a b bad, TaskDispatchment bad a)
performCycle input address (now, actions) (state, _) =
  state
  |> orbiterSnapshotUpdate input actions now
  |> orbiterSnapshotStage input address now
  |> orbiterSnapshotDispatch


{-| todo doc -}
orbitsWithWork : OrbiterInput a b c bad -> OrbiterTask bad a -> OrbiterOutput a b c bad
orbitsWithWork orbiterInput startupTask =
  let
    { inputs, model0, present, stage, update } = orbiterInput


    actionMailbox = Signal.mailbox Lazy.List.empty
    publicAddress = Signal.forwardTo actionMailbox.address Lazy.List.fromList


    aggregatedInputs =
      Lazy.List.foldl
        (Signal.Extra.fairMerge (+++))
        (Signal.constant Lazy.List.empty)
        (actionMailbox.signal ::: inputs)


    inputSignal = Time.timestamp (aggregatedInputs ~> Lazy.List.toList)


    orbiterFold =
      Signal.foldp
        (performCycle orbiterInput publicAddress)
        (dispatchTasks [startupTask] |> orbiterSnapshot model0, dispatchTasks [])
        inputSignal


    orbiterFoldTimestamps = inputSignal ~> fst
    orbiterFoldSnapshots = orbiterFold ~> fst
    orbiterFoldDispatchments = orbiterFold ~> snd


    views = orbiterSnapshotPresent orbiterInput publicAddress <~ orbiterFoldTimestamps ~ orbiterFoldSnapshots


    combinedDispatchments =
      Signal.Extra.fairMerge
        -- model (update/stage) tasks go before view (present) tasks
        combineDispatchments
        orbiterFoldDispatchments
        (.dispatchment <~ views)


    -- NOTE IMPORTANT: This filter is crucial to avoiding a busy loop!
    filteredTasks =
      Signal.filterMap
        (\dispatchment -> if dispatchmentHasWork dispatchment then Just (dispatchmentTask dispatchment) else Nothing)
        nilTask
        combinedDispatchments


    outputConfig =
      { view' = views ~> .view'
      , model' = orbiterFoldSnapshots ~> .model'
      , tasks = filteredTasks
      , now = inputSignal ~> fst
      , actions = inputSignal ~> snd
      , address = publicAddress
      , lazyAddress = actionMailbox.address
      -- TODO factor the current time in to the orbiter _output_
      }

  in
    outputConfig


-- NOTE : This is an alias for |>, but is intended to make opaque the semantics of an _orbiter_ and
-- it's _taps_. This is done so that one needn't reason about function application at all to install
-- taps. Instead, think of +--> as meaning "and then flows in to".

{-| todo doc -}
(+-->) : OrbiterOutput a b c bad -> (OrbiterOutput a b c bad -> OrbiterOutput a b c bad) -> OrbiterOutput a b c bad
(+-->) output portal =
    portal output

{-| todo doc -}
thisTap
  :  OrbiterTap bad a
  -> Signal.Address (List a)
  -> OrbiterOutput a b c bad
  -> OrbiterOutput a b c bad
thisTap tap address output =
  { output | tasks = output.tasks ~> (tap address) }


{-| todo doc -}
thisForwardTap : OrbiterTap bad a -> Signal.Address target -> (List a -> target) -> OrbiterOutput a b c bad -> OrbiterOutput a b c bad
thisForwardTap tap address f output =
  thisTap tap (Signal.forwardTo address f) output

{-| todo doc -}
itself : OrbiterOutput a b c bad -> OrbiterOutput a b c bad
itself output =
  thisTap successTap output.address output

{-| todo doc -}
itselfDeferred : OrbiterOutput a b c bad -> OrbiterOutput a b c bad
itselfDeferred output =
  thisTap asyncSuccessTap output.address output

{-| todo doc -}
thisAddress : Signal.Address (List a) -> OrbiterOutput a b c bad -> OrbiterOutput a b c bad
thisAddress address output =
  thisTap successTap address output

{-| todo doc -}
thisCustomAddress : Signal.Address target -> (List a -> target) -> OrbiterOutput a b c bad -> OrbiterOutput a b c bad
thisCustomAddress address f output =
  thisForwardTap successTap address f output

{-| todo doc -}
thisAddressDeferred : Signal.Address (List a) -> OrbiterOutput a b c bad -> OrbiterOutput a b c bad
thisAddressDeferred address output =
  thisTap asyncSuccessTap address output

{-| todo doc -}
thisCustomAddressDeferred : Signal.Address target -> (List a -> target) -> OrbiterOutput a b c bad -> OrbiterOutput a b c bad
thisCustomAddressDeferred address f output =
  thisForwardTap asyncSuccessTap address f output


--atomicSuccessTap : Signal.Address (List a) -> OrbiterTask bad a -> OrbiterTask bad a

{-| todo doc -}
thisErrorTap : Signal.Address (List a) -> (bad -> List a) -> OrbiterOutput a b c bad -> OrbiterOutput a b c bad
thisErrorTap address handler output =
  thisTap (flip errorTap handler) address output

{-| todo doc -}
it'sErrorTap : (bad -> List a) -> OrbiterOutput a b c bad -> OrbiterOutput a b c bad
it'sErrorTap handler output =
  thisErrorTap output.address handler output
