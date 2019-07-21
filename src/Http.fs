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

let failDecoder = 
        let decodeTuple = Decode.tuple2 Decode.string (Decode.list Decode.string)
        decodeTuple

type HttpResult<'S> =
                | Correct of 'S
                | Failed of string*(string list)


let private decode (successDecoder:Decode.Decoder<'a>) (failDecoder:Decode.Decoder<string*(string list)>) text = 
        let decoder = Decode.field "code" Decode.string 
                        |> Decode.andThen(
                                       function
                                        |"ok" -> Decode.map Correct (Decode.field "data" successDecoder)
                                        |"error" -> Decode.map Failed (Decode.field "data" failDecoder)
                                        | _ -> failwith "NotImplemented"
                        )

        match Decode.fromString decoder text with 
                        |Result.Ok ok -> ok
                        |Error r ->failwithf "decoder error %s" r


let post<'a>  url (successDecoder:Decode.Decoder<'a>) body= 
    
    promise {
        let defaultProps =
              [
                Method HttpMethod.POST;
                Fetch.requestHeaders [ ContentType "application/json" ]
                Body(body |> unbox) ]

        let! response = Fetch.postRecord (sprintf "http://localhost:8080/%s"url) body defaultProps
        let! text = response.text();
        
        return decode successDecoder failDecoder text

    }

let getWithAuthorize<'a>  url (successDecoder:Decode.Decoder<'a>)  jwt = 
    
    promise {
        let defaultProps =
              [
                Method HttpMethod.GET;
                Fetch.requestHeaders [ ContentType "application/json"; Authorization jwt ]
              ]

        let! response = Fetch.fetch (sprintf "http://localhost:8080/%s"url) defaultProps
        let! text = response.text();
        
        return decode successDecoder failDecoder text

    }