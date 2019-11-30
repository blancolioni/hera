with Hera.Calendar;
with Hera.Corporations;

package Hera.Managers is

   type Root_Manager_Type is abstract tagged private;

   procedure Execute
     (Manager : in out Root_Manager_Type)
   is abstract;

   procedure Set_Next_Update_Time
     (Manager     : in out Root_Manager_Type'Class;
      Update_Time : Hera.Calendar.Time);

   procedure Set_Next_Update_Delay
     (Manager      : in out Root_Manager_Type'Class;
      Update_Delay : Duration);

   type Manager_Type is access all Root_Manager_Type'Class;

   procedure Start_Manager
     (Corporation : Hera.Corporations.Corporation_Type);

private

   type Root_Manager_Type is abstract tagged
      record
         Active      : Boolean;
         Next_Update : Hera.Calendar.Time;
      end record;

end Hera.Managers;
