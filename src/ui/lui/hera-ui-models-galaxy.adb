with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Lui.Colors;
with Lui.Rendering;

with Hera.Star_Systems;

with Hera.Real_Images;
with Hera.Logging;

package body Hera.UI.Models.Galaxy is

   Zoom_Limit    : constant := 5.0;
   Zoom_Duration : constant Duration := 0.8;

   Zoomed_Out_Radius : constant := 4;
   Zoomed_In_Radius  : constant := 8;

   Galaxy_Radius_X   : constant := 100.0;
   Galaxy_Radius_Y   : constant := 100.0;

   function Ship_Count_Image
     (Count : Natural)
      return String;

   type Rendered_Star_System_Reference is new Natural;
   subtype Rendered_Star_System_Index is
     Rendered_Star_System_Reference
   range 1 .. Rendered_Star_System_Reference'Last;

   type Rendered_Jump_Gate is
      record
         To_Index : Rendered_Star_System_Index;
         Size     : Non_Negative_Real;
      end record;

   package Jump_Gate_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Rendered_Jump_Gate);

   type Rendered_Star_System is
      record
         Index       : Rendered_Star_System_Index;
         Star_System : Hera.Star_Systems.Star_System_Type;
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Image       : Ada.Strings.Unbounded.Unbounded_String;
         Jumps       : Jump_Gate_Lists.List;
         Color       : Lui.Colors.Color_Type;
         Capital     : Boolean;
         Colony      : Boolean;
      end record;

   package Rendered_Star_System_Vectors is
     new Ada.Containers.Vectors
       (Rendered_Star_System_Index, Rendered_Star_System);

   type Root_Galaxy_Model is
     new Hera.UI.Models.Root_Hera_Model with
      record
         Show_Capital_Names      : Boolean := True;
         Show_System_Names       : Boolean := False;
         Rendered_Star_Systems   : Rendered_Star_System_Vectors.Vector;
         Needs_Render            : Boolean := True;
         Zoomed_To_System        : Boolean := False;
         Zooming_In              : Boolean := False;
         Zooming_Out             : Boolean := False;
         Zoom_Start              : Ada.Calendar.Time;
         Zoom_Level              : Float := 0.0;
         Selected_Star_System    : Hera.Star_Systems.Star_System_Type;
      end record;

   overriding procedure Update
     (Model : in out Root_Galaxy_Model);

   overriding procedure Render
     (Model    : in out Root_Galaxy_Model;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class;
      Layer     : Lui.Render_Layer);

   overriding procedure On_Model_Removed
     (Model : in out Root_Galaxy_Model;
      Child : not null access Lui.Models.Root_Object_Model'Class)
   is null;

   overriding procedure Select_XY
     (Model : not null access Root_Galaxy_Model;
      X, Y  : Natural);

   overriding procedure On_Key_Press
     (Model : in out Root_Galaxy_Model;
      Key   : Character);

   overriding procedure Zoom
     (Model   : in out Root_Galaxy_Model;
      Z       : in     Integer;
      X, Y    : in     Integer;
      Control : in     Boolean);

   overriding function Tooltip
     (Model : Root_Galaxy_Model;
      X, Y  : Natural)
      return String;

   overriding procedure After_Transition
     (Model : in out Root_Galaxy_Model)
   is null;

   procedure Load
     (Model : in out Root_Galaxy_Model'Class);

   procedure Get_Screen_Position
     (Model              : Root_Galaxy_Model'Class;
      X, Y               : Real;
      Screen_X, Screen_Y : out Integer);

   procedure Create_Model
     (Model : in out Root_Galaxy_Model'Class);

   procedure Draw_Connection
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      A, B     : Positive)
     with Unreferenced;

   procedure Draw_Ships
     (Model         : in out Root_Galaxy_Model'Class;
      Renderer      : in out Lui.Rendering.Root_Renderer'Class;
      Star_System   : Hera.Star_Systems.Star_System_Type;
      System_Radius : Positive)
   is null
     with Unreferenced;

   procedure Draw_History
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
     with Unreferenced;

   function Closest_System
     (Model        : Root_Galaxy_Model'Class;
      X, Y         : Integer;
      Max_Distance : Natural)
      return Hera.Star_Systems.Star_System_Type;

   function Get_Star_System_Index
     (Model       : Root_Galaxy_Model'Class;
      Star_System : Hera.Star_Systems.Star_System_Type)
      return Rendered_Star_System_Reference
     with Unreferenced;

--     function Find_Image_Resource
--       (Star_System : Hera.Star_Systems.Star_System_Type)
--        return String;

   function Interpolate
     (Model      : Root_Galaxy_Model'Class;
      Zoomed_Out : Float;
      Zoomed_In  : Float)
      return Float
   is (Zoomed_In * Model.Zoom_Level + Zoomed_Out * (1.0 - Model.Zoom_Level));

   function Zoomed_Size
     (Model           : Root_Galaxy_Model'Class;
      Zoomed_Out_Size : Integer;
      Zoomed_In_Size  : Integer)
      return Integer
   is (if Model.Zooming_In or else Model.Zooming_Out
       then Integer (Model.Interpolate (Float (Zoomed_In_Size),
                                        Float (Zoomed_Out_Size)))
       elsif Model.Zoomed_To_System then Zoomed_In_Size
       else Zoomed_Out_Size);

   procedure Start_Zoom_In
     (Model  : in out Root_Galaxy_Model'Class;
      Target : Hera.Star_Systems.Star_System_Type);

   procedure Start_Zoom_Out
     (Model  : in out Root_Galaxy_Model'Class);

   procedure Clear_Zoom
     (Model : in out Root_Galaxy_Model'Class);

   --     Unexplored_Color : constant Lui.Colors.Color_Type :=
--                           (0.5, 0.5, 0.5, 0.6);
--     Border_Color     : constant Lui.Colors.Color_Type :=
--                           (1.0, 1.0, 1.0, 1.0);

   type Galaxy_Model_Access is access all Root_Galaxy_Model'Class;

   ----------------------
   -- After_Transition --
   ----------------------

--     overriding procedure After_Transition
--       (Model : in out Root_Galaxy_Model)
--     is
--     begin
--        Model.Add_Inline_Model
--          (Width         => Model.Width,
--           Height        => Model.Height,
--           Model         =>
--             Hera.Systems.Models.System_Model (Model.Selected_System),
--           Attach_Left   => True,
--           Attach_Right  => True,
--           Attach_Top    => True,
--           Attach_Bottom => True);
--     end After_Transition;

   ----------------
   -- Clear_Zoom --
   ----------------

   procedure Clear_Zoom
     (Model  : in out Root_Galaxy_Model'Class)
   is
   begin
      Model.Zoom_Level := (if Model.Zooming_In then 1.0 else 0.0);
      Model.Zooming_In := False;
      Model.Zooming_Out := False;
   end Clear_Zoom;

   --------------------
   -- Closest_System --
   --------------------

   function Closest_System
     (Model        : Root_Galaxy_Model'Class;
      X, Y         : Integer;
      Max_Distance : Natural)
      return Hera.Star_Systems.Star_System_Type
   is
      Shortest_Distance   : Natural := Natural'Last;
      Closest_Star_System : Hera.Star_Systems.Star_System_Type;
      Screen_X, Screen_Y  : Integer;
   begin

      for Render of Model.Rendered_Star_Systems loop
         Model.Get_Screen_Position
           (Render.Star_System.X, Render.Star_System.Y, Screen_X, Screen_Y);

         if Screen_X in 1 .. Model.Width
           and then Screen_Y in 1 .. Model.Height
           and then abs (X - Screen_X) <= Shortest_Distance
           and then abs (Y - Screen_Y) <= Shortest_Distance
         then
            declare
               D : constant Natural :=
                     (X - Screen_X) ** 2 + (Y - Screen_Y) ** 2;
            begin
               if D < Shortest_Distance then
                  Shortest_Distance := D;
                  Closest_Star_System := Render.Star_System;
               end if;
            end;
         end if;
      end loop;

      if Max_Distance ** 2 >= Shortest_Distance then
         return Closest_Star_System;
      else
         return null;
      end if;
   end Closest_System;

   ------------------
   -- Create_Model --
   ------------------

   procedure Create_Model
     (Model : in out Root_Galaxy_Model'Class)
   is
   begin
      Model.Drag_Rotation_Behaviour;

      Model.Set_Eye_Position (0.0, 0.0, 2.2);
      Model.Show_Capital_Names := True;
      Model.Show_System_Names := True;

      Model.Load;

   end Create_Model;

   ---------------------
   -- Draw_Connection --
   ---------------------

   procedure Draw_Connection
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      A, B     : Positive)
   is null;
--        use Hera.Db;
--
--        use Hera.Factions;
--        use Hera.Systems;
--        A_System : constant Star_System := Galaxy_Graph.Vertex (A);
--        B_System : constant Star_System := Galaxy_Graph.Vertex (B);
--        A_Owner  : constant Hera.Factions.Faction := A_System.Owner;
--        B_Owner  : constant Hera.Factions.Faction := B_System.Owner;
--
--        Link_Color    : constant Lui.Colors.Color_Type :=
--                           (if A_Owner /= null and then B_Owner = A_Owner
--                            then A_Owner.Color
--                            elsif A_Owner = null or else B_Owner = null
--                            then Unexplored_Color
--                            else Border_Color);
--        Link_Width     : constant Positive :=
--                           Natural'Min
--                             (A_System.Traffic (B_System)
--                              + B_System.Traffic (A_System),
--                              5)
--                           + 1;
--        X1, X2, Y1, Y2 : Integer;
--     begin
--        Model.Star_System_Screen (A_System, X1, Y1);
--        Model.Star_System_Screen (B_System, X2, Y2);
--        Renderer.Draw_Line (X1, Y1, X2, Y2, Link_Color, Link_Width);
--     end Draw_Connection;

   ------------------
   -- Draw_History --
   ------------------

   procedure Draw_History
     (Model    : in out Root_Galaxy_Model'Class;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is null;

   ------------------
   -- Galaxy_Model --
   ------------------

   function Galaxy_Model
      return Hera_Model
   is
      Model : constant Galaxy_Model_Access :=
                new Root_Galaxy_Model;
   begin
      Model.Initialise ("galaxy", 1);
      Model.Create_Model;
      return Hera_Model (Model);
   end Galaxy_Model;

   -------------------------
   -- Get_Screen_Position --
   -------------------------

   procedure Get_Screen_Position
     (Model              : Root_Galaxy_Model'Class;
      X, Y               : Real;
      Screen_X, Screen_Y : out Integer)
   is
      Local_X : Real := X / Galaxy_Radius_X;
      Local_Y : Real := Y / Galaxy_Radius_Y;
   begin
      if Model.Zooming_In
        or else Model.Zooming_Out
        or else Model.Zoomed_To_System
      then
         declare
            Selected : constant Hera.Star_Systems.Star_System_Type :=
              Model.Selected_Star_System;
            Zoomed_X : constant Real :=
                         (X - Selected.X) / Galaxy_Radius_X
                         * 3.0;
            Zoomed_Y : constant Real :=
                         (Y - Selected.Y) / Galaxy_Radius_Y
                         * 3.0;
         begin
            if Model.Zooming_In or else Model.Zooming_Out then
               Local_X :=
                 Real (Model.Interpolate (Float (Zoomed_X),
                       Float (Local_X)));
               Local_Y :=
               Real (Model.Interpolate (Float (Zoomed_Y),
                     Float (Local_Y)));
            else
               Local_X := Zoomed_X;
               Local_Y := Zoomed_Y;
            end if;
         end;
      end if;

      Screen_X := Integer (Local_X * Real (Model.Width / 2))
        + Model.Width / 2;
      Screen_Y := Integer (Local_Y * Real (Model.Height / 2))
        + Model.Height / 2;

      declare
         use Hera.Star_Systems;
         function Img (X : Real) return String
                       renames Hera.Real_Images.Approximate_Image;
      begin
         if Model.Selected_Star_System /= null
           and then not Model.Zooming_In
           and then not Model.Zooming_Out
           and then X = Model.Selected_Star_System.X
         then
            Hera.Logging.Log
              ("gtk",
               "selected xy: "
               & Img (Model.Selected_Star_System.X)
               & " " & Img (Model.Selected_Star_System.Y)
               & "; local xy: "
               & Img (Local_X)
               & " " & Img (Local_Y)
               & "; x=" & Img (X) & " y=" & Img (Y)
               & "; screen xy =" & Screen_X'Image & Screen_Y'Image);
         end if;
      end;

   end Get_Screen_Position;

   ---------------------------
   -- Get_Star_System_Index --
   ---------------------------

   function Get_Star_System_Index
     (Model       : Root_Galaxy_Model'Class;
      Star_System : Hera.Star_Systems.Star_System_Type)
      return Rendered_Star_System_Reference
   is
      use type Hera.Star_Systems.Star_System_Type;
   begin
      for I in 1 .. Model.Rendered_Star_Systems.Last_Index loop
         if Model.Rendered_Star_Systems.Element (I).Star_System
           = Star_System
         then
            return I;
         end if;
      end loop;
      return 0;
   end Get_Star_System_Index;

   ----------
   -- Load --
   ----------

   procedure Load
     (Model : in out Root_Galaxy_Model'Class)
   is
      Next_Index : Rendered_Star_System_Index := 1;

      procedure Append
        (Star_System : Hera.Star_Systems.Star_System_Type);

      ------------
      -- Append --
      ------------

      procedure Append
        (Star_System : Hera.Star_Systems.Star_System_Type)
      is
         Rec      : constant Rendered_Star_System :=
           Rendered_Star_System'
             (Index       => Next_Index,
              Star_System => Star_System,
              Name        =>
                Ada.Strings.Unbounded.To_Unbounded_String
                  (Star_System.Name),
              Image       =>
                Ada.Strings.Unbounded.Null_Unbounded_String,
              Color       => Lui.Colors.White,
              Colony      => Star_System.Primary.Colonized,
              Capital     => False,
              Jumps  => <>);

      begin
         Model.Rendered_Star_Systems.Append (Rec);
         Next_Index := Next_Index + 1;
      end Append;

   begin
      Model.Rendered_Star_Systems.Clear;

      Hera.Star_Systems.Iterate (Append'Access);
   end Load;

   ------------------
   -- On_Key_Press --
   ------------------

   overriding procedure On_Key_Press
     (Model : in out Root_Galaxy_Model;
      Key   : Character)
   is
   begin
      if Key = Ada.Characters.Latin_1.ESC then
         if Model.Zoomed_To_System then
            Model.Start_Zoom_Out;
         elsif Model.Zooming_In then
            Model.Clear_Zoom;
         end if;
      end if;
   end On_Key_Press;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model     : in out Root_Galaxy_Model;
      Renderer  : in out Lui.Rendering.Root_Renderer'Class;
      Layer     : Lui.Render_Layer)
   is
      use type Lui.Render_Layer;

      System_Radius : constant Natural :=
        Model.Zoomed_Size
          (Zoomed_Out_Radius, Zoomed_In_Radius);

      procedure Draw_Star
        (Star_System : Hera.Star_Systems.Star_System_Type;
         X, Y        : Integer);

      ---------------
      -- Draw_Star --
      ---------------

      procedure Draw_Star
        (Star_System : Hera.Star_Systems.Star_System_Type;
         X, Y        : Integer)
      is
         pragma Unreferenced (Star_System);
         Radius : constant Non_Negative_Real :=
           Non_Negative_Real (System_Radius);
      begin
         Renderer.Circle
           (X      => X,
            Y      => Y,
            Radius => Natural (Radius),
            Filled => True);
      end Draw_Star;

   begin
      if Layer = 1 then
         for Star_Pass in Boolean loop
            for System of Model.Rendered_Star_Systems loop
               declare
                  use type Hera.Star_Systems.Star_System_Type;
                  Screen_X, Screen_Y : Integer;
               begin
                  Model.Get_Screen_Position
                    (System.Star_System.X, System.Star_System.Y,
                     Screen_X, Screen_Y);

                  if not Model.Zoomed_To_System
                    or else Model.Zooming_In
                    or else Model.Zooming_Out
                    or else Model.Selected_Star_System
                      = System.Star_System
                    or else (Screen_X in 1 .. Model.Width
                             and then Screen_Y in 1 .. Model.Height)
                  then

                     if Star_Pass then
                        Renderer.Set_Color (System.Color);
                        Draw_Star (System.Star_System, Screen_X, Screen_Y);

                        if System.Colony then
                           Renderer.Set_Color (System.Color);
                           Renderer.Set_Line_Width (2.0);
                           Renderer.Circle
                             (X          => Screen_X,
                              Y          => Screen_Y,
                              Radius     => System_Radius * 2,
                              Filled     => False);

                        end if;

                        declare
                           Name : constant String :=
                                    Ada.Strings.Unbounded.To_String
                                      (System.Name);
                        begin
                           if Model.Show_System_Names
                             or else (System.Capital
                                      and then Model.Show_Capital_Names)
                           then
                              Renderer.Set_Color
                                (Lui.Colors.To_Color
                                   (100, 100, 100));
                              Renderer.Set_Font
                                ("Tahoma", 16.0);
                              Renderer.Text
                                (X      => Screen_X - 4 * Name'Length,
                                 Y      => Screen_Y + 42,
                                 Value  => Name);
                           end if;
                        end;

                     else

                        for Jump of System.Jumps loop
                           declare
                              To          : Rendered_Star_System renames
                                              Model.Rendered_Star_Systems
                                                (Jump.To_Index);
                              To_X        : constant Real :=
                                              To.Star_System.X;
                              To_Y        : constant Real :=
                                              To.Star_System.Y;
                              To_Screen_X : Integer;
                              To_Screen_Y : Integer;
                           begin
                              Model.Get_Screen_Position
                                (To_X, To_Y, To_Screen_X, To_Screen_Y);
                              Renderer.Set_Color (0.4, 0.4, 0.4, 0.8);
                              Renderer.Set_Line_Width (Jump.Size);
                              Renderer.Line
                                (X1         => Screen_X,
                                 Y1         => Screen_Y,
                                 X2         => To_Screen_X,
                                 Y2         => To_Screen_Y);
                           end;
                        end loop;
                     end if;
                  end if;
               end;
            end loop;

--              if Star_Pass then
--                 if True or else Model.Zoomed_To_System then
--                    Model.Rendered_Stacks.Render (Model, Renderer);
--                 end if;
--              end if;

         end loop;

      end if;

      Model.Needs_Render := False;

--      Hera.Updates.End_Render;

   end Render;

   ---------------
   -- Select_XY --
   ---------------

   overriding procedure Select_XY
     (Model : not null access Root_Galaxy_Model;
      X, Y  : Natural)
   is
      use Hera.Star_Systems;
      Closest : constant Star_System_Type :=
                 Model.Closest_System
                   (X, Y, Zoomed_In_Radius);
   begin
      if Closest = null then
         if Model.Zoomed_To_System then
            Model.Start_Zoom_Out;
         end if;
      else
         if Model.Zoomed_To_System then
            if Closest = Model.Selected_Star_System then
               null;
--                 Hera.UI.Models.Top.Top_Model
--                   (Model.Parent_Model)
--                   .Show_Star_System (Star_System);
            else
               Model.Selected_Star_System := Closest;
               Model.Needs_Render := True;
            end if;
         else
            Model.Start_Zoom_In (Closest);
         end if;
      end if;

      Model.Set_Render_Layer_Changed (1);

   end Select_XY;

   ----------------------
   -- Ship_Count_Image --
   ----------------------

   function Ship_Count_Image
     (Count : Natural)
      return String
   is
   begin
      if Count < 10 then
         return Result : constant String (1 .. Count) := (others => 'I') do
            null;
         end return;
      elsif Count < 100 then
         declare
            Xs : constant String (1 .. Count / 10) := (others => 'X');
         begin
            return Xs & Ship_Count_Image (Count mod 10);
         end;
      else
         declare
            Result : constant String := Natural'Image (Count);
         begin
            return Result (2 .. Result'Last);
         end;
      end if;
   end Ship_Count_Image;

   -------------------
   -- Start_Zoom_In --
   -------------------

   procedure Start_Zoom_In
     (Model  : in out Root_Galaxy_Model'Class;
      Target : Hera.Star_Systems.Star_System_Type)
   is
   begin
      Model.Zooming_In := True;
      Model.Zooming_Out := False;
      Model.Zoom_Start := Ada.Calendar.Clock;
      Model.Zoom_Level := 0.0;
      Model.Selected_Star_System := Target;
   end Start_Zoom_In;

   --------------------
   -- Start_Zoom_Out --
   --------------------

   procedure Start_Zoom_Out
     (Model  : in out Root_Galaxy_Model'Class)
   is
   begin
      Model.Zooming_In := False;
      Model.Zooming_Out := True;
      Model.Zoomed_To_System := False;
      Model.Zoom_Level := 1.0;
      Model.Zoom_Start := Ada.Calendar.Clock;
--        Model.Rendered_Stacks.Clear;
   end Start_Zoom_Out;

   -------------
   -- Tooltip --
   -------------

   overriding function Tooltip
     (Model : Root_Galaxy_Model;
      X, Y  : Natural)
      return String
   is
      use Hera.Star_Systems;
      Closest : constant Star_System_Type :=
                 Model.Closest_System (X, Y, 30);
--        Stack  : constant Stack_Type := null;
--                   Model.Rendered_Stacks.Find_Stack (X, Y);
   begin
--        if Stack /= null then
--           return Stack.Owner.Name;
--        elsif Star_System /= null then
      if Closest /= null then
         return Closest.Name;
      else
         return "";
      end if;
   end Tooltip;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Model : in out Root_Galaxy_Model)
   is
   begin
      if Model.Zooming_In or else Model.Zooming_Out then
         declare
            use Ada.Calendar;
         begin
            Model.Zoom_Level :=
              Float (Clock - Model.Zoom_Start) / Float (Zoom_Duration);
            if Model.Zooming_In then
               Model.Zoom_Level := 1.0 - Model.Zoom_Level;
            end if;

            if Model.Zoom_Level not in 0.0 .. 1.0 then
               if Model.Zooming_In then
                  Model.Zoomed_To_System := True;
               else
                  Model.Zoomed_To_System := False;
               end if;
               Model.Clear_Zoom;
            end if;
         end;
         Model.Set_Render_Layer_Changed (1);
      end if;

   end Update;

   ----------
   -- Zoom --
   ----------

   overriding procedure Zoom
     (Model   : in out Root_Galaxy_Model;
      Z       : in     Integer;
      X, Y    : in     Integer;
      Control : in     Boolean)
   is
   begin
      Lui.Models.Root_Object_Model (Model).Zoom (Z, X, Y, Control);
      if Model.Eye_Z > Zoom_Limit then
         Model.Set_Eye_Position (Model.Eye_X, Model.Eye_Y, Zoom_Limit);
      end if;
   end Zoom;

end Hera.UI.Models.Galaxy;
