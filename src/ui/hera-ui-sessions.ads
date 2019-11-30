private package Hera.UI.Sessions is

   function Exists (Id : String) return Boolean;

   procedure New_Session
     (Id    : String;
      State : State_Interface'Class)
     with Pre => not Exists (Id);

   procedure Close_Session
     (Id : String)
     with Pre => Exists (Id);

   procedure Close_All_Sessions;

   procedure Scan_Active_Sessions
     (Process : not null access
        procedure (State : State_Interface'Class));

   function Reference
     (Id : String)
      return access State_Interface'Class
     with Pre => Exists (Id);

   function Is_Active
     (User : UI_Account)
      return Boolean;

   function Element
     (User : UI_Account)
      return access constant State_Interface'Class;

   procedure Broadcast
     (Signal : Hera.Signals.Signal_Type);

end Hera.UI.Sessions;
