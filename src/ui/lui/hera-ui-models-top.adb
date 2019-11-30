with Tropos.Reader;

with Hera.Calendar;
with Hera.Paths;

with Hera.UI.Models.Galaxy;

package body Hera.UI.Models.Top is

   Class_Id : constant String := "top";

   Layout_Config : Tropos.Configuration;
   Have_Layout   : Boolean := False;

   UI_Layer          : constant Lui.Render_Layer := 1;
   Last_Render_Layer : constant Lui.Render_Layer := UI_Layer;

   subtype Top_Model_Render_Layer is
     Lui.Render_Layer range 1 .. Last_Render_Layer;

   procedure Load_Layout
     (Model : in out Top_Hera_Model'Class);

   procedure Draw
     (Model     : Top_Hera_Model'Class;
      Rectangle : Lui.Layout_Rectangle;
      Color     : Lui.Colors.Color_Type;
      Filled    : Boolean;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class);

   ----------------------
   -- Create_Top_Model --
   ----------------------

   function Create_Top_Model
      return Top_Model
   is
   begin
      if not Have_Layout then
         Layout_Config :=
           Tropos.Reader.Read_Config
             (Hera.Paths.Config_File ("ui/layout.txt"));
         Have_Layout := True;
      end if;

      declare
         Top : constant Top_Model := new Top_Hera_Model;
      begin
         Top.Initialize_Model;
         return Top;
      end;
   end Create_Top_Model;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Model     : Top_Hera_Model'Class;
      Rectangle : Lui.Layout_Rectangle;
      Color     : Lui.Colors.Color_Type;
      Filled    : Boolean;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class)
   is
      pragma Unreferenced (Model);
   begin
      Renderer.Set_Color (Color);
      Renderer.Rectangle (Rectangle, Filled);
   end Draw;

   ----------------------
   -- Initialize_Model --
   ----------------------

   procedure Initialize_Model
     (Model   : not null access Top_Hera_Model'Class)
   is
   begin
      Model.Initialise
        (Name              => Class_Id,
         Last_Render_Layer => Last_Render_Layer);
      Model.Set_Background (Lui.Colors.Black);
   end Initialize_Model;

   -----------------
   -- Load_Layout --
   -----------------

   procedure Load_Layout
     (Model : in out Top_Hera_Model'Class)
   is
      use Lui;

      function Rectangle (Name : String) return Layout_Rectangle;

      ---------------
      -- Rectangle --
      ---------------

      function Rectangle (Name : String) return Layout_Rectangle is
         Config : constant Tropos.Configuration :=
                    Layout_Config.Child (Name);
         Left   : constant Integer := Config.Get (1);
         Top    : constant Integer := Config.Get (2);
         Right  : constant Integer := Config.Get (3);
         Bottom : constant Integer := Config.Get (4);
         X1     : constant Integer :=
                    (if Left < 0 then Model.Width + Left else Left);
         Y1     : constant Integer :=
                    (if Top < 0 then Model.Height + Top else Top);
         X2     : constant Integer :=
                    (if Left < 0 or else Right < 0
                     then Model.Width - abs Right else Right);
         Y2     : constant Integer :=
                    (if Top < 0 or else Bottom < 0
                     then Model.Height - abs Bottom else Bottom);
      begin
         return Layout_Rectangle'
           (X      => X1,
            Y      => Y1,
            Width  => Integer'Max (X2 - X1, 1),
            Height => Integer'Max (Y2 - Y1, 1));
      end Rectangle;

   begin
      Model.Left_Toolbar_Layout := Rectangle ("left-toolbar");
      Model.Top_Toolbar_Layout := Rectangle ("top-toolbar");
      Model.Bottom_Toolbar_Layout := Rectangle ("bottom-toolbar");
      Model.Mini_Map_Layout := Rectangle ("mini-map");
      Model.Main_Rectangle := Rectangle ("main-map");
      Model.Status_Layout := Rectangle ("status-box");
      Model.Selected_Stack_Layout := Rectangle ("selected-stack-layout");
      Model.Sidebar_Icon_Size := Layout_Config.Get ("sidebar-icon-size", 64);

      Model.Layout_Loaded := True;
   end Load_Layout;

   ------------------
   -- On_Key_Press --
   ------------------

   overriding procedure On_Key_Press
     (Model : in out Top_Hera_Model;
      Key   : Character)
   is
   begin
      Model.Current_Model.On_Key_Press (Key);
   end On_Key_Press;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Top_Hera_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      Layer    : Lui.Render_Layer)
   is

      procedure Render_Static_UI;

      ----------------------
      -- Render_Static_UI --
      ----------------------

      procedure Render_Static_UI is
      begin
         Renderer.Image
           (Rec      => (0, 0, Model.Width, Model.Height),
            Resource => "ui-background");

         Model.Draw
           (Rectangle => Model.Status_Layout,
            Color => (0.2, 0.2, 0.2, 1.0),
            Filled => True,
            Renderer => Renderer);

         Renderer.Set_Color (Lui.Colors.White);
         Renderer.Set_Font ("Tahoma", 20.0);

         Renderer.Text
           (Model.Status_Layout.X + 4,
            Model.Status_Layout.Y + 24,
            Hera.Calendar.Image (Hera.Calendar.Clock));

      end Render_Static_UI;

   begin
      if not Model.Layout_Loaded then
         Model.Load_Layout;
      end if;

      case Top_Model_Render_Layer (Layer) is
         when UI_Layer =>
            Render_Static_UI;
      end case;

   end Render;

   ------------
   -- Resize --
   ------------

   overriding procedure Resize
     (Item          : in out Top_Hera_Model)
   is
   begin
      Lui.Models.Root_Object_Model (Item).Resize;
      Item.Layout_Loaded := False;
   end Resize;

   -----------------------
   -- Set_Current_Model --
   -----------------------

   procedure Set_Current_Model
     (Top         : not null access Top_Hera_Model'Class;
      New_Current : Hera_Model)
   is
   begin
      if not Top.Layout_Loaded then
         Top.Load_Layout;
      end if;

      if Top.Current_Model /= null then
         Top.Remove_Inline_Model (Top.Current_Model);
      end if;

      Top.Current_Model := New_Current;

      Top.Add_Offset_Model
        (Model         => Top.Current_Model,
         Left_Offset   => Top.Left_Toolbar_Layout.Width + 10,
         Top_Offset    => Top.Top_Toolbar_Layout.Height + 10,
         Right_Offset  => 10,
         Bottom_Offset => Top.Bottom_Toolbar_Layout.Height + 10);

      --        if Model.Current_Model.Minimap_Model /= null then
      --           Model.Add_Inline_Model
      --         (Model.Mini_Map_Layout, Model.Current_Model.Minimap_Model);
      --        end if;
   end Set_Current_Model;

   -----------------
   -- Show_Galaxy --
   -----------------

   procedure Show_Galaxy
     (Model : not null access Top_Hera_Model'Class)
   is
   begin
      if Model.Galaxy_Model = null then
         Model.Galaxy_Model :=
           Hera.UI.Models.Galaxy.Galaxy_Model;
      end if;

      Model.Set_Current_Model (Model.Galaxy_Model);

   end Show_Galaxy;

end Hera.UI.Models.Top;
