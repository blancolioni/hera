with Hera.Json;
with Hera.Signals;

package Hera.UI is

   Signal_Clock_Tick : constant Hera.Signals.Signal_Type :=
     "signal-clock-tick";

   type Client_Id is new Natural;

   type Connection_Interface is interface
     and Hera.Signals.Signal_Data_Interface;

   procedure Send_Message
     (Connection : Connection_Interface;
      Message    : Hera.Json.Json_Value'Class)
   is abstract;

   type UI_Account_Interface is interface;

   type UI_Account is access all UI_Account_Interface'Class;

   function Is_Administrator
     (Account : UI_Account_Interface)
      return Boolean
      is abstract;

   function User_Name
     (Account : UI_Account_Interface)
      return String
      is abstract;

   type UI_Interface is interface;

   procedure Start
     (Item  : UI_Interface)
   is abstract;

   procedure Stop
     (Item    : UI_Interface;
      Message : String)
   is abstract;

   procedure Wait
     (Item  : UI_Interface)
   is abstract;

   procedure Broadcast
     (UI     : UI_Interface;
      Signal : Hera.Signals.Signal_Type)
   is abstract;

   function Current_UI return UI_Interface'Class;

   type State_Interface is interface
     and Hera.Signals.Signaler;

   function Valid
     (State : State_Interface)
      return Boolean
      is abstract;

   function User_Name
     (State : State_Interface)
      return String
   is abstract
     with Pre'Class => State.Valid;

   function User_Account
     (State : State_Interface)
      return UI_Account
      is abstract
     with Pre'Class => State.Valid;

   function Is_Administrator
     (State : State_Interface)
      return Boolean
      is abstract
     with Pre'Class => State.Valid;

   function New_Client
     (State          : in out State_Interface;
      Model_Name     : String;
      Model_Argument : String)
      return Client_Id
      is abstract;

   procedure Replace_Model
     (State          : in out State_Interface;
      Client         : Client_Id;
      Model_Name     : String;
      Model_Argument : String)
      is abstract;

   procedure Close_Client
     (State  : in out State_Interface;
      Client : Client_Id)
   is abstract;

   procedure Set_Connection
     (State      : in out State_Interface;
      Connection : Connection_Interface'Class)
   is abstract;

   function Handle_Message
     (State      : in out State_Interface;
      Message    : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
      is abstract;

   procedure Send_Message
     (State   : State_Interface;
      Message : Hera.Json.Json_Value'Class)
   is abstract;

   procedure After_Update
     (State : State_Interface)
   is abstract;

   function Execute_Command
     (State   : in out State_Interface;
      Client  : Client_Id;
      Command : String)
      return Hera.Json.Json_Value'Class
   is abstract;

   function Handle_Client_Post
     (State   : in out State_Interface;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is abstract;

   function Handle_Client_Get
     (State   : State_Interface;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
      is abstract;

   function Status_Value
     (State : State_Interface;
      Name  : String)
      return Hera.Json.Json_Value'Class
      is abstract;

   procedure Set_Status_Value
     (State : in out State_Interface;
      Name  : String;
      Value : Hera.Json.Json_Value'Class)
   is abstract;

   function Environment_Value
     (State : State_Interface;
      Name  : String)
      return Hera.Json.Json_Value'Class
   is abstract;

   function Environment_Value
     (State : State_Interface'Class;
      Name  : String)
      return String
   is (State.Environment_Value (Name).Image);

   procedure Broadcast (Signal : Hera.Signals.Signal_Type);
   procedure Send_State;

   procedure After_Update;

   procedure Close_All;

private

   procedure On_UI_Started (UI : UI_Interface'Class);

   type Message_Flags is (Cash_Changed);
   type Message_Flag_Array is array (Message_Flags) of Boolean;

   type User_Message is tagged
      record
         User   : UI_Account;
         Flags  : Message_Flag_Array := (others => False);
      end record;

   function New_Message
     (User : UI_Account)
      return User_Message
   is (User_Message'
         (User => User,
          Flags       => <>));

--     function Cash_Changed
--       (Message : Corporation_Message)
--        return Corporation_Message
--     is ((Message with delta
--           Flags => (Message.Flags with delta Cash_Changed => True)));

end Hera.UI;
