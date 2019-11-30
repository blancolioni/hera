with Hera.Sessions;

package body Hera.UI.Web_UI.Handlers.Start is

   -------------------
   -- Handle_Create --
   -------------------

   overriding function Handle_Create
     (Handler    : Start_Handler;
      Parameters : Routes.Parameter_Container'Class)
      return State_Interface'Class
   is
      pragma Unreferenced (Handler, Parameters);
   begin
      return Hera.Sessions.New_Session;
   end Handle_Create;

end Hera.UI.Web_UI.Handlers.Start;
