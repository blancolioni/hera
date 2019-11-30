with Hera.UI.Web_UI;

package body Hera.UI.Launch is

   ------------
   -- Get_UI --
   ------------

   function Get_UI (Name : String) return UI_Interface'Class is
      pragma Unreferenced (Name);
   begin
      return Hera.UI.Web_UI.Get_Web_UI;
   end Get_UI;

end Hera.UI.Launch;
