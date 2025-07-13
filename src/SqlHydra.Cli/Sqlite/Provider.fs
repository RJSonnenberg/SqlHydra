module SqlHydra.Sqlite.Provider

open SqlHydra.Domain

let provider = 
    {
        Provider.Id = "sqlite"
        Provider.Name = "SqlHydra.Sqlite"
        Provider.Type = Sqlite
        Provider.DefaultReaderType = "System.Data.Common.DbDataReader" // "System.Data.IDataReader" 
        Provider.DefaultProvider = "System.Data.SQLite"
        Provider.GetSchema = SqliteSchemaProvider.getSchema
    }
