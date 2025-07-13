module SqlHydra.MySql.Provider

open SqlHydra.Domain

let provider =
    {
        Provider.Id = "mysql"
        Provider.Name = "SqlHydra.MySql"
        Provider.Type = MySql
        Provider.DefaultReaderType = "System.Data.Common.DbDataReader" // "System.Data.IDataReader"
        Provider.DefaultProvider = "MySql.Data"
        Provider.GetSchema = MySqlSchemaProvider.getSchema
    }
