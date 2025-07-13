﻿module SqlHydra.Program

open System
open FSharp.SystemCommandLine
open Input
open Console
open Domain

let run (provider: Provider, tomlFile: IO.FileInfo option, project: IO.FileInfo option, connString: string option) =

    let tomlFile = defaultArg tomlFile (IO.FileInfo($"sqlhydra-{provider.Id}.toml"))
    
    let projectOrFirstFound =
        project
        |> Option.map (fun p -> if p.Exists then p else failwith $"Unable to find the specified project file: '{p.FullName}'.")
        |> Option.orElse (IO.DirectoryInfo(".").EnumerateFiles("*.fsproj") |> Seq.tryHead)
        |> Option.defaultWith (fun () -> failwith "Unable to find a .fsproj file in the run directory. Please specify one using the `--project` option.")

    {
        Provider = provider
        TomlFile = tomlFile
        Project = projectOrFirstFound
        Version = Version.get()
        ConnectionString = connString
    }
    |> Console.run

[<EntryPoint>]
let main argv =
    rootCommand argv {
        description "SqlHydra.Cli"
        inputs (
            argument "provider" 
            |> required 
            |> desc "The database provider id: 'mssql', 'npgsql', 'sqlite', 'mysql', or 'oracle'"
            |> tryParse (fun res ->
                match res.Tokens[0].Value with
                | "mssql" ->  Ok SqlServer.Provider.provider
                | "npgsql" -> Ok Npgsql.Provider.provider
                | "sqlite" -> Ok Sqlite.Provider.provider
                | "mysql" ->  Ok MySql.Provider.provider
                | "oracle" -> Ok Oracle.Provider.provider
                | providerId -> Error $"Invalid provider id: '{providerId}'. Valid options are: 'mssql', 'npgsql', 'sqlite', 'mysql', or 'oracle'."
            ),
            
            optionMaybe "--toml-file" 
            |> alias "-t" 
            |> desc "The toml configuration filename. Default: 'sqlhydra-{provider}.toml'",
            
            optionMaybe "--project" 
            |> alias "-p" 
            |> desc "The project file to update. If not configured, the first .fsproj found in the run directory will be used.",
            
            optionMaybe "--connection-string" 
            |> alias "-cs" 
            |> desc "The DB connection string to use. This will override the connection string in the toml file."
        )
        setAction run
    }
