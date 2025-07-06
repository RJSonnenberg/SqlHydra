module SqlHydra.Program

open System
open FSharp.SystemCommandLine
open Input
open Console

type Provider = 
    | SqlServer
    | Npgsql
    | Sqlite
    | MySql
    | Oracle

let handler (provider: Provider, tomlFile: IO.FileInfo option, project: IO.FileInfo option, connString: string option) =

    let providerInfo, getSchema =
        match provider with
        | SqlServer -> SqlServer.AppInfo.info, SqlServer.SqlServerSchemaProvider.getSchema
        | Npgsql -> Npgsql.AppInfo.info, Npgsql.NpgsqlSchemaProvider.getSchema
        | Sqlite -> Sqlite.AppInfo.info, Sqlite.SqliteSchemaProvider.getSchema
        | MySql -> MySql.AppInfo.info, MySql.MySqlSchemaProvider.getSchema
        | Oracle -> Oracle.AppInfo.info, Oracle.OracleSchemaProvider.getSchema

    let projectOrFirstFound =
        project
        |> Option.map (fun p -> if p.Exists then p else failwith $"Unable to find the specified project file: '{p.FullName}'.")
        |> Option.orElse (IO.DirectoryInfo(".").EnumerateFiles("*.fsproj") |> Seq.tryHead)
        |> Option.defaultWith (fun () -> failwith "Unable to find a .fsproj file in the run directory. Please specify one using the `--project` option.")

    let args : Console.Args =
        {
            AppInfo = providerInfo
            GetSchema = getSchema
            TomlFile = tomlFile |> Option.defaultWith (fun () -> IO.FileInfo($"sqlhydra-{provider}.toml"))
            Project = projectOrFirstFound
            Version = Version.get()
            ConnectionString = connString
        }

    Console.run args

[<EntryPoint>]
let main argv =
    rootCommand argv {
        description "SqlHydra.Cli"
        inputs (
            argument "provider" 
            |> required 
            |> desc "The database provider name: 'mssql', 'npgsql', 'sqlite', 'mysql', or 'oracle'"
            |> tryParse (fun res ->
                match res.Tokens[0].Value with
                | "mssql" -> Ok SqlServer
                | "npgsql" -> Ok Npgsql
                | "sqlite" -> Ok Sqlite
                | "mysql" -> Ok MySql
                | "oracle" -> Ok Oracle
                | provider -> Error $"Invalid db provider: '{provider}'. Valid options are: 'mssql', 'npgsql', 'sqlite', 'mysql', or 'oracle'."
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
        setAction handler
    }
