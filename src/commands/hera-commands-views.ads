private with Hera.UI.Views;

private package Hera.Commands.Views is

   procedure Load_View_Commands;

private

   type Load_View_Command is
     abstract new Root_Hera_Command with null record;

   function Create_View
     (Command   : Load_View_Command;
      Session   : Hera.Sessions.Hera_Session;
      Arguments : Argument_List)
      return Hera.UI.Views.View_Type
      is abstract;

   overriding procedure Perform
     (Command   : Load_View_Command;
      Session   : Hera.Sessions.Hera_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List);

end Hera.Commands.Views;
