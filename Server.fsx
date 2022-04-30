open System.Threading
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

type Information = 
    | Info of (int)
    

//configuration
let configuration = 
    ConfigurationFactory.ParseString(
        @"akka {
            log-config-on-start : on
            stdout-loglevel : DEBUG
            loglevel : ERROR
            actor {
                provider = ""Akka.Remote.RemoteActorRefProvider, Akka.Remote""
                debug : {
                    receive : on
                    autoreceive : on
                    lifecycle : on
                    event-stream : on
                    unhandled : on
                }
            }
            remote {
                helios.tcp {
                    transport-protocol=tcp
                    port = 9000
                    hostname = 10.20.108.19
                }
            }
        }")

let system = ActorSystem.Create("server", configuration)



//Server coin Mine Actors - 
type ServerCoinMiner() =
    inherit Actor()
    override x.OnReceive message =
        let x : Information = downcast message
        let ip= fsi.CommandLineArgs.[1] |> string
        
        //let sref = select "akka.tcp://rem@"+ ip +":9000/user/Printer" system
        let sref= system.ActorSelection("akka.tcp://client@"+ip+":9000/user/Printer")
        match x with
        | Info(k) -> 
            
            printfn "entered worker"
            let n =8



            let ran = Random()
                 
            let l = message 
            let mutable str = ""
            let chars = "ABCDEFGHIJKLMNOPQRSTUVWUXYZabcdefghijklmnopqrstuvwxyz0123456789"
            let mutable count = 0
                
            for i in 1 .. n do
                count <- (ran.Next() % 62)
                    
                str <- String.concat "" [str; chars.[count].ToString()]

            let result= "rmarri"+str
                    
            
            let by = System.Text.Encoding.ASCII.GetBytes result

            let strconv= by |> (new SHA256Managed()).ComputeHash |> System.BitConverter.ToString 

             

            let res = strconv.Replace("-", "")

            let coinArr= res.ToCharArray()

            let res1= result+ "  "

            let res2= res1+ res

            let final= "coin found in remote system" + res2
            let mutable count= 0 

            
            for i in 0..k-1 do

                if coinArr.[i]='0' then

                    count<-count+1 

                         
            
            if count=k then 
                sref <! final
                sref <! "done"
            
                

//Remote Boss Actor - Receives subproblem from the client machine and assigns it to the child actors in the same machine.
//It maintains an actor pool based on the cores' count and allocates subproblems in the Round-Robin approach. 
let ServerMainActor = 
    spawn system "ServerMainActor"
    <| fun mailbox ->
        let actcount = System.Environment.ProcessorCount |> int64
        let tot = actcount*1L
        printfn "%d" tot
        let serverCoinPool = 
                [1L .. tot]
                |> List.map(fun id -> system.ActorOf(Props(typedefof<ServerCoinMiner>)))

        let minerenum = [|for rp in serverCoinPool -> rp|]
        let minerSystem = system.ActorOf(Props.Empty.WithRouter(Akka.Routing.RoundRobinGroup(minerenum)))
        //workerSystem <! Info(2)
        
        let rec loop() =
            actor {
                let! (message:obj) = mailbox.Receive()
                printfn "I am in this"
                // let (startindex,k,endindex) : Tuple<int64,int64,int64> = downcast message
                let k = downcast message
                printf "in here and k is %i" k
                minerSystem <! Info(k)
                return! loop()
            }
        printf "Server Miner actor Started \n" 
        loop()


System.Console.ReadLine()

