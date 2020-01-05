[<AutoOpen>]
module Utils

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json

[<RequireQualifiedAccess>]
module Literals =
    let [<Literal>] STORAGE_KEY = "sanuki-compiler"

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
