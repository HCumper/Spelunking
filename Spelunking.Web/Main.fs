module Spelunking.Web.Main

open System.Threading.Tasks
open Bolero
open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Spelunking.Web

type MyApp() =
    inherit Component()

    let initialModel, _ = App.init ()

    [<Inject>]
    member val JS: IJSRuntime = Unchecked.defaultof<_> with get, set

    member val private KeyboardReference: DotNetObjectReference<MyApp> option = None with get, set
    member val private Model = initialModel with get, set

    member private this.DispatchMessage(message: App.Message) =
        let nextModel, _ = App.update message this.Model
        this.Model <- nextModel
        this.StateHasChanged()

    [<JSInvokable>]
    member this.HandleKeyDown(key: string) =
        this
            .InvokeAsync(fun () ->
                this.DispatchMessage(App.KeyDown key))

    override this.OnAfterRenderAsync(_firstRender) =
        Spelunking.Web.Storage.configure this.JS

        let reference =
            match this.KeyboardReference with
            | Some reference -> reference
            | None ->
                let reference = DotNetObjectReference.Create(this)
                this.KeyboardReference <- Some reference
                reference

        Task.WhenAll(
            this.JS.InvokeVoidAsync("spelunk.setKeyboardReceiver", reference).AsTask(),
            this.JS.InvokeVoidAsync("spelunk.installKeyboard").AsTask(),
            this.JS.InvokeVoidAsync("spelunk.focusGame").AsTask())

    override this.Render() =
        Render.view this.Model this.DispatchMessage
