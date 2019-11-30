private with Ada.Calendar;

with WL.String_Maps;

with Lui.Rendering;

package Hera.UI.Models.Top is

   type Top_Hera_Model is
     new Lui.Models.Root_Object_Model with private;

   type Top_Model is access all Top_Hera_Model'Class;

   function Create_Top_Model
      return Top_Model;

   overriding procedure Render
     (Model    : in out Top_Hera_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      Layer    : Lui.Render_Layer);

--     overriding procedure Update
--       (Model    : in out Top_Hera_Model);

   overriding procedure Resize
     (Item : in out Top_Hera_Model);

   procedure Initialize_Model
     (Model : not null access Top_Hera_Model'Class);

   function Wizard_Mode
     (Model : Top_Hera_Model'Class)
      return Boolean;

   procedure Show_Galaxy
     (Model : not null access Top_Hera_Model'Class);

private

   package Model_Maps is
     new WL.String_Maps (Hera_Model);

   type Top_Hera_Model is
     new Lui.Models.Root_Object_Model with
      record
         Wizard                : Boolean := False;
         Layout_Loaded         : Boolean := False;
         Left_Toolbar_Layout   : Lui.Layout_Rectangle;
         Top_Toolbar_Layout    : Lui.Layout_Rectangle;
         Bottom_Toolbar_Layout : Lui.Layout_Rectangle;
         Main_Rectangle        : Lui.Layout_Rectangle;
         Mini_Map_Layout       : Lui.Layout_Rectangle;
         Status_Layout         : Lui.Layout_Rectangle;
         Selected_Stack_Layout : Lui.Layout_Rectangle;
         Sidebar_Icon_Size     : Positive := 64;
         Galaxy_Model          : Hera_Model;
         Planet_Models         : Model_Maps.Map;
         Current_Model         : Hera_Model;
         Previous_Update       : Ada.Calendar.Time;
      end record;

   overriding procedure On_Key_Press
     (Model : in out Top_Hera_Model;
      Key   : Character);

   procedure Set_Current_Model
     (Top         : not null access Top_Hera_Model'Class;
      New_Current : Hera_Model);

   function Wizard_Mode
     (Model : Top_Hera_Model'Class)
      return Boolean
   is (Model.Wizard);

end Hera.UI.Models.Top;
