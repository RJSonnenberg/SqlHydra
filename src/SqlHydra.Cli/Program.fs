module SqlHydra.Program

open System
open FSharp.SystemCommandLine
open Input

let handler (provider: string, tomlFile: IO.FileInfo option, project: IO.FileInfo option, connString: string option) =

    let info, getSchema =
        match provider with
        | "mssql" -> SqlServer.AppInfo.info, SqlServer.SqlServerSchemaProvider.getSchema
        | "npgsql" -> Npgsql.AppInfo.info, Npgsql.NpgsqlSchemaProvider.getSchema
        | "sqlite" -> Sqlite.AppInfo.info, Sqlite.SqliteSchemaProvider.getSchema
        | "mysql" -> MySql.AppInfo.info, MySql.MySqlSchemaProvider.getSchema
        | "oracle" -> Oracle.AppInfo.info, Oracle.OracleSchemaProvider.getSchema
        | _ -> failwith "Unsupported db provider. Valid options are: 'mssql', 'npgsql', 'sqlite', 'mysql', or 'oracle'."

    let projectOrFirstFound =
        project
        |> Option.map (fun p -> if p.Exists then p else failwith $"Unable to find the specified project file: '{p.FullName}'.")
        |> Option.orElse (IO.DirectoryInfo(".").EnumerateFiles("*.fsproj") |> Seq.tryHead)
        |> Option.defaultWith (fun () -> failwith "Unable to find a .fsproj file in the run directory. Please specify one using the `--project` option.")

    let args : Console.Args =
        {
            Provider = provider
            AppInfo = info
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
            argument "provider" |> required |> desc "The database provider name: 'mssql', 'npgsql', 'sqlite', 'mysql', or 'oracle'",
            optionMaybe "--toml-file" |> alias "-t" |> desc "The toml configuration filename. Default: 'sqlhydra-{provider}.toml'",
            optionMaybe "--project" |> alias "-p" |> desc "The project file to update. If not configured, the first .fsproj found in the run directory will be used.",
            optionMaybe "--connection-string" |> alias "-cs" |> desc "The DB connection string to use. This will override the connection string in the toml file."
        )
        setAction handler
    }
