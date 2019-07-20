module Http
open Fable.PowerPack.Fetch.Fetch_types
open Thoth.Json
open Fable.PowerPack.PromiseImpl
open Fable.PowerPack

type Response<'T>= 
        {
                status:string
                data:'T       
        }

let failDecoder<'t> = 
        let decodeTuple = Decode.tuple2 Decode.string (Decode.list Decode.string)
        decodeTuple

type PostResult<'S,'F> =
                | Correct of 'S
                | Failed of 'F 

let post<'a,'b>  url (successDecoder:Decode.Decoder<'a>) (failDecoder:Decode.Decoder<'b>) body= 
    
    promise {
        let defaultProps =
              [
                RequestProperties.Method HttpMethod.POST;
                Fetch.requestHeaders [ ContentType "application/json" ]
                RequestProperties.Body(body |> unbox) ]

        let! response = Fetch.postRecord url body defaultProps
        let! text = response.text();
        
        let decoder = Decode.field "code" Decode.string 
                        |> Decode.andThen(
                                function
                                        |"ok" -> Decode.map Correct (Decode.field "data" successDecoder)
                                        |"error" -> Decode.map Failed (Decode.field "data" failDecoder)
                        )
        
        return (match Decode.fromString decoder text with 
                        |Result.Ok ok -> ok
                        |Error r ->failwithf "decoder error %s" r)

    }