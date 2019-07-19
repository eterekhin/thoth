module Http
open Fable.PowerPack.Fetch.Fetch_types
open Thoth.Json
open Fable.PowerPack.PromiseImpl
open Fable.PowerPack
let post  url decoder body= 
    promise {
        let defaultProps =
              [
                RequestProperties.Method HttpMethod.POST;
                Fetch.requestHeaders [ ContentType "application/json" ]
                RequestProperties.Body(body |> unbox) ]

        let! response = Fetch.postRecord url body defaultProps
        let! text = response.text();
        let encode = Decode.fromString  decoder text
        return encode;
    }