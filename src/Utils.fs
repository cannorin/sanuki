[<AutoOpen>]
module Utils

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json

let [<Global>] private setTimeout(f: unit->unit, ms: int): unit = jsNative

type GenericObservable<'T>(?disp: unit->unit) =
  let listeners = Dictionary<Guid, IObserver<'T>>()
  member __.Trigger v =
    for lis in listeners.Values do
      lis.OnNext v
  interface IObservable<'T> with
    member __.Subscribe w =
      let g = Guid.NewGuid()
      listeners.Add(g, w)
      { new IDisposable with
          member __.Dispose() =
            match disp with
            | Some disp -> disp()
            | None -> ()
            listeners.Remove(g) |> ignore }

let createObservable(subscribe: ('T->unit)->unit) : GenericObservable<'T> =
  let obs = GenericObservable()
  subscribe obs.Trigger
  obs

let debounce (ms: int) (obs: IObservable<'T>): IObservable<'T> =
  let mutable timeoutActive = false
  let mutable snapshot = Unchecked.defaultof<'T>
  let mutable disposable: IDisposable option = None
  let debouncedObs = GenericObservable(fun () ->
      disposable |> Option.iter(fun d -> d.Dispose()))
  disposable <-
      obs |> Observable.subscribe (fun ev ->
        snapshot <- ev
        if not timeoutActive then
          timeoutActive <- true
          setTimeout((fun () ->
              debouncedObs.Trigger(snapshot)
              timeoutActive <- false), ms)) |> Some
  upcast debouncedObs

type ObservableWorker<'InMsg>(worker: obj, decoder: Decoder<'InMsg>, ?name: string) =
  let name = defaultArg name "FABLE WORKER"
  let listeners = new Dictionary<Guid, IObserver<'InMsg>>()
  do worker?addEventListener("message", fun ev ->
    match ev?data: obj with
    | :? string as msg when not(String.IsNullOrEmpty(msg)) ->
      match Decode.fromString decoder msg with
      | Ok msg ->
        // JS.console.log("[" + name + "] Received:", msg)
        for listener in listeners.Values do
            listener.OnNext(msg)
      | Error err -> JS.console.error("[" + name + "] Cannot decode:", err)
    | _ -> ())
  member __.Worker = worker
  member __.HasListeners =
    listeners.Count > 0
  member inline this.Post(msg: 'OutMsg): unit =
    this.Worker?postMessage(Encode.Auto.toString(0, msg))
  member inline this.PostAndAwaitResponse(msg: 'OutMsg, picker: 'InMsg -> 'Res option): Async<'Res> =
    Async.FromContinuations(fun (cont, err, cancel) ->
      let mutable disp = Unchecked.defaultof<IDisposable>
      disp <- this |> Observable.subscribe(fun msg ->
        match picker msg with
        | Some res ->
            disp.Dispose()
            cont res
        | None -> ())
      this.Worker?postMessage(Encode.Auto.toString(0, msg))
    )
  member __.Subscribe obs =
    let id = Guid.NewGuid()
    listeners.Add(id, obs)
    { new IDisposable with
        member __.Dispose() = listeners.Remove(id) |> ignore }
  interface IObservable<'InMsg> with
    member this.Subscribe obs = this.Subscribe(obs)
