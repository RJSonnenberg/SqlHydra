module SqlHydra.Npgsql.Provider

open SqlHydra.Domain

let provider = 
    {
        Provider.Id = "npgsql"
        Provider.Name = "SqlHydra.Npgsql"
        Provider.Type = Npgsql
        Provider.DefaultReaderType = "Npgsql.NpgsqlDataReader"
        Provider.DefaultProvider = "Npgsql"
        Provider.GetSchema = NpgsqlSchemaProvider.getSchema
    }
