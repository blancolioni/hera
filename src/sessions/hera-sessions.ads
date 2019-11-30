private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Ordered_Maps;
private with WL.String_Maps;

private with Hera.Json;
private with Hera.Writers;

private with Hera.Corporations;

with Ada.Finalization;

with Hera.Objects;

with Hera.Contexts;
with Hera.Signals;
with Hera.UI;
with Hera.UI.Models;

package Hera.Sessions is

   function New_Session
     (User_Name : String;
      Password  : String)
     return Hera.UI.State_Interface'Class;

   function New_Administrator_Session
      return Hera.UI.State_Interface'Class;

   type Root_Hera_Session is
     new Ada.Finalization.Controlled
     and Hera.UI.State_Interface with private;

   subtype Hera_Session is Root_Hera_Session'Class;

   function Default_Context
     (Session : Root_Hera_Session'Class)
      return Hera.Contexts.Context_Type;

private

   type Client_Type is
      record
         Model   : Hera.UI.Models.Hera_Model;
         Context : Hera.Contexts.Context_Type;
      end record;

   package Client_Maps is
     new Ada.Containers.Ordered_Maps
       (Hera.UI.Client_Id, Client_Type, Hera.UI."<");

   package Environment_Maps is
     new WL.String_Maps (Hera.Json.Json_Value'Class, Hera.Json."=");

   type Context_Updater is access
     procedure (Context : in out Hera.Contexts.Context_Type);

   package Connection_Holders is
     new Ada.Containers.Indefinite_Holders
       (Hera.UI.Connection_Interface'Class,
        Hera.UI."=");

   package Changed_Object_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Hera.Objects.Hera_Object, Hera.Objects."=");

   protected type Session_Data is

      procedure Create_Client
        (User           : Hera.UI.UI_Account;
         Context        : Hera.Contexts.Context_Type;
         Model_Name     : String;
         Model_Argument : String;
         Client_Id      : out Hera.UI.Client_Id);

      procedure Close_Client
        (Client_Id      : Hera.UI.Client_Id);

      procedure Scan_Clients
        (Process : not null access
           procedure
             (Client : Hera.UI.Client_Id;
              Model  : in out Hera.UI.Models.Root_Hera_Model'Class));

      procedure Execute_Command
        (Client_Id : Hera.UI.Client_Id;
         Writer    : in out Hera.Writers.Writer_Interface'Class;
         Command   : String);

      function Get_Model
        (Client_Id : Hera.UI.Client_Id)
         return Hera.UI.Models.Hera_Model;

      procedure Set_Model
        (Client_Id : Hera.UI.Client_Id;
         Model     : Hera.UI.Models.Hera_Model);

      procedure Set_Environment_Value
        (Name : String;
         Value : Json.Json_Value'Class);

      function Get_Environment_Value
        (Name : String)
         return Json.Json_Value'Class;

      procedure Object_Changed
        (Object : Hera.Objects.Hera_Object);

      procedure Scan_Changed_Objects
        (Process : not null access
           procedure (Object : Hera.Objects.Hera_Object));

      procedure Reference;
      procedure Unreference (Finished : out Boolean);

   private

      References      : Natural := 1;
      Last_Client     : Hera.UI.Client_Id := 0;
      Client_Map      : Client_Maps.Map;
      Environment     : Environment_Maps.Map;
      Changed_Objects : Changed_Object_Lists.List;
   end Session_Data;

   type Session_Data_Access is access Session_Data;

   type Session_Watcher is
     new Hera.Objects.Watcher_Interface with
      record
         Data : Session_Data_Access;
      end record;

   overriding procedure On_Object_Changed
     (Watcher : Session_Watcher;
      Object  : Hera.Objects.Hera_Object);

   package Session_Watcher_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Session_Watcher);

   type Root_Hera_Session is
     new Ada.Finalization.Controlled
     and Hera.UI.State_Interface with
      record
         User             : Hera.UI.UI_Account;
         Corporation      : Hera.Corporations.Corporation_Type;
         Default_Context  : Hera.Contexts.Context_Type;
         Connection       : Connection_Holders.Holder;
         Dispatcher       : Hera.Signals.Signal_Dispatcher;
         On_Clock_Tick_Id : Hera.Signals.Handler_Id;
         Watchers         : Session_Watcher_Lists.List;
         Data             : Session_Data_Access;
      end record;

   overriding procedure Initialize (Session : in out Root_Hera_Session);
   overriding procedure Finalize (Session : in out Root_Hera_Session);
   overriding procedure Adjust (Session : in out Root_Hera_Session);

   overriding function Valid
     (Session   : Root_Hera_Session)
      return Boolean;

   overriding function Is_Administrator
     (Session   : Root_Hera_Session)
      return Boolean;

   overriding function User_Name
     (Session   : Root_Hera_Session)
      return String;

   overriding function User_Account
     (Session   : Root_Hera_Session)
      return Hera.UI.UI_Account;

   overriding function New_Client
     (Session        : in out Root_Hera_Session;
      Model_Name     : String;
      Model_Argument : String)
      return Hera.UI.Client_Id;

   overriding procedure Replace_Model
     (Session        : in out Root_Hera_Session;
      Client         : Hera.UI.Client_Id;
      Model_Name     : String;
      Model_Argument : String);

   overriding procedure Close_Client
     (Session   : in out Root_Hera_Session;
      Client    : Hera.UI.Client_Id);

   overriding procedure Set_Connection
     (Session    : in out Root_Hera_Session;
      Connection : Hera.UI.Connection_Interface'Class);

   overriding function Handle_Message
     (Session    : in out Root_Hera_Session;
      Message    : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class;

   overriding procedure After_Update
     (Session : Root_Hera_Session);

   overriding procedure Send_Message
     (Session : Root_Hera_Session;
      Message : Hera.Json.Json_Value'Class);

   overriding function Execute_Command
     (Session : in out Root_Hera_Session;
      Client  : Hera.UI.Client_Id;
      Command : String)
      return Hera.Json.Json_Value'Class;

   overriding function Handle_Client_Get
     (Session : Root_Hera_Session;
      Client  : Hera.UI.Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class;

   overriding function Handle_Client_Post
     (Session : in out Root_Hera_Session;
      Client  : Hera.UI.Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class;

   overriding procedure Send_Signal
     (Session : in out Root_Hera_Session;
      Signal  : Hera.Signals.Signal_Type);

   overriding function Add_Handler
     (Session : in out Root_Hera_Session;
      Signal  : Hera.Signals.Signal_Type;
      Handler : Hera.Signals.Handler_Type;
      Data    : Hera.Signals.Signal_Data_Interface'Class)
     return Hera.Signals.Handler_Id;

   overriding procedure Remove_Handler
     (Session : in out Root_Hera_Session;
      Signal  : Hera.Signals.Signal_Type;
      Id      : Hera.Signals.Handler_Id);

   overriding function Status_Value
     (Session : Root_Hera_Session;
      Name    : String)
      return Hera.Json.Json_Value'Class;

   overriding procedure Set_Status_Value
     (Session : in out Root_Hera_Session;
      Name    : String;
      Value   : Hera.Json.Json_Value'Class);

   overriding function Environment_Value
     (Session : Root_Hera_Session;
      Name  : String)
      return Hera.Json.Json_Value'Class;

   function Status_Message
     (Session : Root_Hera_Session'Class)
      return Json.Json_Object;

   overriding function User_Account
     (Session   : Root_Hera_Session)
      return Hera.UI.UI_Account
   is (Session.User);

   function Default_Context
     (Session : Root_Hera_Session'Class)
      return Hera.Contexts.Context_Type
   is (Session.Default_Context);

end Hera.Sessions;
