with Hera.Corporations.Updates;

with Hera.Random;

package body Hera.Managers.Colonies is

   type Root_Colony_Manager is
     new Root_Manager_Type with
      record
         Corporation : Hera.Corporations.Corporation_Type;
         Colony      : Hera.Colonies.Colony_Type;
      end record;

   overriding procedure Execute
     (Manager : in out Root_Colony_Manager);

   --------------------
   -- Colony_Manager --
   --------------------

   function Colony_Manager
     (Corporation : Hera.Corporations.Corporation_Type;
      Colony      : Hera.Colonies.Colony_Type)
      return Manager_Type
   is
      use Hera.Calendar;
   begin
      return new Root_Colony_Manager'
        (Active       => True,
         Next_Update  => Hera.Calendar.Clock + Days (Hera.Random.Unit_Random),
         Corporation  => Corporation,
         Colony       => Colony);
   end Colony_Manager;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Manager : in out Root_Colony_Manager)
   is
   begin
      Hera.Corporations.Updates.Sell_Commodities
        (Corporation => Manager.Corporation,
         Colony      => Manager.Colony);
      Manager.Set_Next_Update_Delay
        (Update_Delay => Hera.Calendar.Days (1));
   end Execute;

end Hera.Managers.Colonies;
