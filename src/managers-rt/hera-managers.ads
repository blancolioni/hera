private with WL.String_Maps;

with Hera.Calendar;
with Hera.Updates;

with Hera.Db.Managed;

package Hera.Managers is

   subtype Priority_Type is Integer range 1 .. 20;

   Top_Priority        : constant Priority_Type := 1;
   Emergency_Priority  : constant Priority_Type := 3;
   High_Priority       : constant Priority_Type := 6;
   Medium_Priority     : constant Priority_Type := 10;
   Low_Priority        : constant Priority_Type := 15;
   Lowest_Priority     : constant Priority_Type := Priority_Type'Last;

   type Root_Manager_Type is abstract tagged private;

   function Identifier
     (Manager : Root_Manager_Type)
      return String
      is abstract;

   procedure Activate
     (Manager : not null access Root_Manager_Type)
   is abstract;

   procedure Log
     (Manager : Root_Manager_Type;
      Message : String);

   procedure Set_Next_Update_Time
     (Manager     : not null access Root_Manager_Type'Class;
      Update_Time : Hera.Calendar.Time);

   procedure Set_Next_Update_Delay
     (Manager      : not null access Root_Manager_Type'Class;
      Update_Delay : Duration);

   type Manager_Type is access all Root_Manager_Type'Class;

   type Constructor_Function is access
     function (Managed : Hera.Db.Managed_Reference)
               return Manager_Type;

   procedure Signal
     (Managed : Hera.Db.Managed_Reference);

private

   type Root_Manager_Type is abstract tagged
      record
         Managed         : Hera.Db.Managed_Reference :=
                             Hera.Db.Null_Managed_Reference;
         Is_Active       : Boolean := False;
         Has_Next_Update : Boolean := False;
         Signaled        : Boolean := False;
         Next_Update     : Hera.Calendar.Time;
      end record;

   package Register_Maps is
     new WL.String_Maps (Constructor_Function);

   Register : Register_Maps.Map;

   function Active_Key
     (Managed : Hera.Db.Managed_Reference)
      return String;

   package Manager_Maps is
     new WL.String_Maps (Manager_Type);

   Active_Map : Manager_Maps.Map;

   package Active_Middle_Maps is
     new WL.String_Maps (Manager_Type);

   type Manager_Update is
     new Hera.Updates.Update_Interface with
      record
         Manager : Manager_Type;
      end record;

   overriding procedure Activate
     (Update : Manager_Update);

   function Managed_Key
     (Managed : Hera.Db.Managed.Managed_Type)
      return String
   is (Managed.Manager & Managed.Identity);

end Hera.Managers;
