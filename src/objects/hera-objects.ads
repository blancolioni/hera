private with Ada.Containers;
private with Ada.Strings.Unbounded;
private with Ada.Strings.Fixed.Hash;

with Tropos;

with Hera.Identifiers;
with Hera.Json;
with Hera.Knowledge;
with Hera.Serialization;

package Hera.Objects is

   Object_Deserialization_Error : exception;

   type Object_Version is new String (1 .. 5);

   type Configured_Interface is interface;

   procedure Load
     (Configured : in out Configured_Interface;
      Config     : Tropos.Configuration)
   is abstract;

   procedure Save
     (Configured : Configured_Interface;
      Config     : in out Tropos.Configuration)
   is abstract;

   type Root_Hera_Object is
     abstract new Configured_Interface
     and Hera.Serialization.Serializable_Interface
     and Hera.Knowledge.Knowable_Interface
   with private;

   function Identifier
     (Object : Root_Hera_Object'Class)
      return Hera.Identifiers.Object_Identifier;

   function Log_Id
     (Object : Root_Hera_Object)
      return String;

   overriding procedure Load
     (Object : in out Root_Hera_Object;
      Config : Tropos.Configuration);

   overriding procedure Save
     (Object : Root_Hera_Object;
      Config : in out Tropos.Configuration);

   overriding procedure Serialize
     (Object          : Root_Hera_Object;
      Detail          : Hera.Serialization.Detail_Level_Type;
      Knowledge       : Hera.Knowledge.Knowledge_Interface'Class;
      Json            : in out Hera.Json.Json_Object'Class);

   type Hera_Object is access constant Root_Hera_Object'Class;

   procedure Initialize
     (Object  : in out Root_Hera_Object'Class;
      Version : Object_Version);

   function Save_Object
     (Object : Root_Hera_Object'Class)
      return Hera_Object;

   function New_Object
     (Object  : Root_Hera_Object;
      Version : Object_Version)
      return Hera_Object;

   procedure Log
     (Object : Root_Hera_Object'Class;
      Message : String);

   type Watcher_Interface is interface;

   procedure On_Object_Changed
     (Watcher : Watcher_Interface;
      Object  : Hera_Object)
   is abstract;

   procedure Add_Watcher
     (To : Root_Hera_Object'Class;
      Watcher : Watcher_Interface'Class);

   procedure Remove_Watcher
     (From    : Root_Hera_Object'Class;
      Watcher : Watcher_Interface'Class);

   procedure Remove_Watcher
     (Watcher : Watcher_Interface'Class);

   type Root_Localised_Object is abstract new Root_Hera_Object with private;

   function Tag (Object : Root_Localised_Object) return String;

   overriding procedure Load
     (Object : in out Root_Localised_Object;
      Config : Tropos.Configuration);

   overriding procedure Save
     (Object : Root_Localised_Object;
      Config : in out Tropos.Configuration);

   overriding procedure Serialize
     (Object          : Root_Localised_Object;
      Detail          : Hera.Serialization.Detail_Level_Type;
      Knowledge       : Hera.Knowledge.Knowledge_Interface'Class;
      Json            : in out Hera.Json.Json_Object'Class);

   function New_Localised_Object
     (Object  : Root_Localised_Object;
      Version : Object_Version;
      Tag     : String)
      return Hera_Object;

   procedure Initialize
     (Object  : in out Root_Localised_Object'Class;
      Version : Object_Version;
      Tag     : String);

   type Root_Named_Object is abstract new Root_Hera_Object with private;

   function Name (Object : Root_Named_Object) return String;

   overriding procedure Load
     (Object : in out Root_Named_Object;
      Config : Tropos.Configuration);

   overriding procedure Save
     (Object : Root_Named_Object;
      Config : in out Tropos.Configuration);

   overriding procedure Serialize
     (Object          : Root_Named_Object;
      Detail          : Hera.Serialization.Detail_Level_Type;
      Knowledge       : Hera.Knowledge.Knowledge_Interface'Class;
      Json            : in out Hera.Json.Json_Object'Class);

   procedure Initialize
     (Object  : in out Root_Named_Object'Class;
      Version : Object_Version;
      Name    : String);

   function New_Named_Object
     (Object  : Root_Named_Object;
      Version : Object_Version;
      Name    : String)
      return Hera_Object;

   type Root_Update_Type is abstract tagged private;

   procedure Create
     (Update : in out Root_Update_Type;
      Target : Root_Hera_Object'Class);

   procedure Execute
     (Update : Root_Update_Type;
      Object : in out Root_Hera_Object'Class)
   is abstract;

   function Description
     (Update : Root_Update_Type)
      return String
      is abstract;

   procedure Add_Update
     (Target : Root_Hera_Object'Class;
      Update : Root_Update_Type'Class);

   procedure Add_Update
     (Target : Hera.Identifiers.Object_Identifier;
      Update : Root_Update_Type'Class);

   procedure Apply_Updates;

private

   type Root_Hera_Object is
     abstract new Configured_Interface
     and Hera.Serialization.Serializable_Interface
     and Hera.Knowledge.Knowable_Interface with
      record
         Version    : Object_Version;
         Identifier : Hera.Identifiers.Object_Identifier;
      end record;

   function Identifier
     (Object : Root_Hera_Object'Class)
      return Hera.Identifiers.Object_Identifier
   is (Object.Identifier);

   function Log_Id
     (Object : Root_Hera_Object)
      return String
   is (String (Object.Identifier));

   type Root_Localised_Object is abstract new Root_Hera_Object with
      record
         Tag : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Log_Id
     (Object : Root_Localised_Object)
      return String
   is (Tag (Object));

   type Root_Named_Object is abstract new Root_Hera_Object with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Log_Id
     (Object : Root_Named_Object)
      return String
   is (Name (Object));

   type Root_Update_Type is abstract tagged
      record
         Target : Hera.Identifiers.Object_Identifier;
      end record;

   function Hash (Id : Hera.Identifiers.Object_Identifier)
                  return Ada.Containers.Hash_Type
   is (Ada.Strings.Fixed.Hash (String (Id)));

end Hera.Objects;
