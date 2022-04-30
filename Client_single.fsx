open System.Threading
open System.Text

#time "on"
#r "nuget: Akka"
#r "nuget: Akka.FSharp"
#r "nuget: Akka.TestKit"
#r "nuget: Akka.Remote"

open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open Akka.TestKit
open System.Security.Cryptography



type InFrCmd = 
    | Info of (int*int)
    | CoinsMined of (string)
    | Info2 of (int*int*int)
    
//system configuration - Enter the local system's ip address at hostname.
let configuration = 
    ConfigurationFactory.ParseString(
        @"akka {
        log-config-on-start : on
        stdout-loglevel : DEBUG
        loglevel : ERROR
        actor {
            provider = ""Akka.Remote.RemoteActorRefProvider, Akka.Remote""
            
        }

        }
    }")

let system= ActorSystem.Create("client", configuration)
let mutable coinsCount = 0

//this actor mines coins by generating random string calculating the SHA 256 and checks if the generated hash is a bitcoin or not.
let coinMine (mailbox: Actor<_>) = 

    let rec loop () = actor {
        let! (message:InFrCmd)= mailbox.Receive ()
        match message with
        | Info( n, k) ->
            // printfn "inside actor"
            //random string generation
            let ran = Random()
            let l = message 
            let mutable str = ""
            let chars = "ABCDEFGHIJKLMNOPQRSTUVWUXYZabcdefghijklmnopqrstuvwxyz0123456789"
            let mutable count = 0
                
            for i in 1 .. n do
                count <- (ran.Next() % chars.Length)
                    
                str <- String.concat "" [str; chars.[count].ToString()]

            let result= "adobra;"+str
                    
            //SHA calculation
            let by = System.Text.Encoding.ASCII.GetBytes result

            let strconv= by |> (new SHA256Managed()).ComputeHash |> System.BitConverter.ToString 

             

            let res = strconv.Replace("-", "")

            //printfn "%s %s" result res

            let coinArr= res.ToCharArray()

            let mutable count= 0 

            //checking if the hash is a bitcoin 
            for i in 0..k-1 do

                if coinArr.[i]='0' then
                    count<-count+1     
            
            let res1= result + " "

            let res2= res1+ res
            if count=k then
                coinsCount <- coinsCount+1
                //printfn "%i" coinsCount
                printfn "coin found in client system %s" res2

                // Terminating the system if the coins count is 2. 
                if coinsCount > 1 then
                    mailbox.Context.System.Terminate() |> ignore 
            


            

        | CoinsMined(_) -> failwith "Not Implemented"
        | _ -> ()
        
        return! loop()
    }
    loop ()

//prints the coins mines from system 2
let printActor (mailbox:Actor<_>) = 
    let rec loop () = actor { 
        let pn=system.ActorSelection("akka.tcp://client@10.20.108.11:9000/user/boss")

        let! (message:string) = mailbox.Receive()
        printfn "%s" message
        return! loop()
    }
    loop()
let printerRef = spawn system "Printer" printActor



let mainActor (mailbox:Actor<_>) = 
    let countofProcessors = System.Environment.ProcessorCount |> int
    let mutable tactors1:int = countofProcessors*6500 //countofProcessors*125   
    let inline spawner c worker = spawn system (sprintf "%i" c) worker
    let actorstoMine = 
            [1 .. tactors1]
            |> List.map(fun c -> spawner c coinMine)
     
    let rec loop() = actor  {

        let! (message:InFrCmd)= mailbox.Receive()

        //enter remote system's ip address here.
        // let servref = system.ActorSelection("akka.tcp://server@10.20.108.19:9000/user/ServerMainActor")
        
        
        
        
        match message with
        | Info(n, k) ->
            printfn "in info ---------------------------------------------------------"
            let inline functionCall x = x <! Info(n, k)

            for i in [0..tactors1-1] do

                if i%2=0 then
                    // printfn "hy"
                    // functionCall (actorstoMine.[i])
                    actorstoMine.Item i <! Info(n,k)
                    
                // else
                //     servref <! (k)


        | CoinsMined(_) -> failwith "Not Implemented"
        | Info2(_) -> failwith "Not Implemented"
        return! loop()
    }
    loop()


        
let b = spawn system "boss" mainActor
let K = fsi.CommandLineArgs.[1] |> int
b <! Info(8,K)

system.WhenTerminated.Wait()