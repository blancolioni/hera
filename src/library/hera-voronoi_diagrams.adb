with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;

with Interfaces.C.Strings;

with Hera.Paths;

package body Hera.Voronoi_Diagrams is

   procedure Write_Sites
     (Path  : String;
      Sites : Site_Vectors.Vector);

   procedure Read_Triangles
     (Path : String;
      Triangles : out Triangle_Vectors.Vector);

   function Find_Circle_Centre
     (P1, P2, P3 : Voronoi_Point)
      return Voronoi_Point;

   procedure Sort_Points
     (Diagram  : Voronoi_Diagram'Class;
      Pts      : in out Point_Index_Array;
      Centre   : Voronoi_Point);

   procedure Calculate_Voronoi_Diagram
     (In_File : Interfaces.C.Strings.chars_ptr;
      Out_File : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Calculate_Voronoi_Diagram);

   procedure Create_Spherical_Cap
     (Diagram   : in out Voronoi_Diagram'Class;
      Triangles : in out Triangle_Vectors.Vector);

   ---------------
   -- Add_Point --
   ---------------

   procedure Add_Point
     (To   : in out Voronoi_Diagram'Class;
      X, Y : Real)
   is
      New_Site : constant Voronoi_Site :=
                   (Point => (X, Y),
                    Site_Index => To.Sites.Last_Index + 1);
   begin
      To.Min_X := Real'Min (To.Min_X, X);
      To.Max_X := Real'Max (To.Max_X, X);
      To.Min_Y := Real'Min (To.Min_Y, Y);
      To.Max_Y := Real'Max (To.Max_Y, Y);

      To.Sites.Append (New_Site);
   end Add_Point;

   -------------------------
   -- Add_Spherical_Point --
   -------------------------

   procedure Add_Spherical_Point
     (To        : in out Voronoi_Diagram'Class;
      X, Y, Z   : Signed_Unit_Real)
   is
   begin
      To.Add_Point (X / (1.0 - Z), Y / (1.0 - Z));
      To.Spherical := True;
   end Add_Spherical_Point;

   -----------
   -- Clear --
   -----------

   procedure Clear (Item : in out Voronoi_Diagram'Class) is
   begin
      Item.Sites.Clear;
      Item.Diagram.Clear;
      Item.Min_X := Real'Last;
      Item.Min_Y := Real'First;
      Item.Max_X := Real'First;
      Item.Max_Y := Real'First;
   end Clear;

   --------------------------
   -- Create_Spherical_Cap --
   --------------------------

   procedure Create_Spherical_Cap
     (Diagram   : in out Voronoi_Diagram'Class;
      Triangles : in out Triangle_Vectors.Vector)
   is
      Hull     : constant Hera.Voronoi_Diagrams.Point_Index_Array :=
                   Diagram.Get_Convex_Hull;
      Previous : Positive := Hull (Hull'Last);
      Cap      : constant Positive := Diagram.Sites.Last_Index + 1;
      Index    : Positive := Diagram.Sites.Last_Index + 1;
   begin
      for H of Hull loop
         Triangles.Append ((Cap, H, Previous));
         Index := Index + 1;
         Previous := H;
      end loop;
   end Create_Spherical_Cap;

   ------------------------
   -- Find_Circle_Centre --
   ------------------------

   function Find_Circle_Centre
     (P1, P2, P3 : Voronoi_Point)
      return Voronoi_Point
   is
      Epsilon : constant := 0.0000001;
      Offset : constant Real := P2.X ** 2 + P2.Y ** 2;
      BC : constant Real := (P1.X ** 2 + P1.Y ** 2 - Offset) / 2.0;
      CD : constant Real := (Offset - P3.X ** 2 - P3.Y ** 2) / 2.0;
      Det : constant Real :=
              (P1.X - P2.X) * (P2.Y - P3.Y)
            - (P2.X - P3.X) * (P1.Y - P2.Y);
      Inverse_Det : constant Real :=
                      (if abs Det >= Epsilon
                       then 1.0 / Det
                       else raise Constraint_Error
                         with "Find_Circle_Centre: (nearly) colinear points");
      Centre_X : constant Real :=
                   (BC * (P2.Y - P3.Y) - CD * (P1.Y - P2.Y)) * Inverse_Det;
      Centre_Y : constant Real :=
                   (CD * (P1.X - P2.X) - BC * (P2.X - P3.X)) * Inverse_Det;
   begin
      return (Centre_X, Centre_Y);
   end Find_Circle_Centre;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Diagram : in out Voronoi_Diagram'Class)
   is
      Site_Path : constant String :=
                    Hera.Paths.Config_File ("voronoi.txt");
      Triangle_Path : constant String :=
                        Hera.Paths.Config_File ("delauny.txt");
      Triangles     : Triangle_Vectors.Vector;

   begin

      Write_Sites (Site_Path, Diagram.Sites);

      Calculate_Voronoi_Diagram
        (Interfaces.C.Strings.New_String (Site_Path),
         Interfaces.C.Strings.New_String (Triangle_Path));

      Read_Triangles (Triangle_Path, Triangles);

      if Diagram.Spherical then
         Diagram.Create_Spherical_Cap (Triangles);
         for T of Triangles loop
            Diagram.Triangles.Append (T);
         end loop;
      else

         for T of Triangles loop
            declare
               Centre : constant Voronoi_Point :=
                          Find_Circle_Centre
                            (Diagram.Sites.Element (T.A).Point,
                             Diagram.Sites.Element (T.B).Point,
                             Diagram.Sites.Element (T.C).Point);
            begin
               Diagram.Diagram_Pts.Append (Centre);
               Diagram.Triangles.Append ((T.A, T.B, T.C));
            end;
         end loop;

         for Site_Index in 1 .. Diagram.Sites.Last_Index loop
            declare
               Site_Points : Point_Index_Array (1 .. 20);
               Count       : Natural := 0;
            begin
               for Triangle_Index in 1 .. Triangles.Last_Index loop
                  declare
                     T : Voronoi_Triangle renames Triangles (Triangle_Index);
                  begin
                     if T.A = Site_Index
                       or else T.B = Site_Index
                       or else T.C = Site_Index
                     then
                        Count := Count + 1;
                        Site_Points (Count) := Triangle_Index;
                     end if;
                  end;
               end loop;

               Diagram.Sort_Points (Site_Points (1 .. Count),
                                    Diagram.Sites.Element (Site_Index).Point);
               Diagram.Diagram.Append ((Count, Site_Points (1 .. Count)));
            end;
         end loop;
      end if;
   end Generate;

   ---------------------
   -- Get_Convex_Hull --
   ---------------------

   function Get_Convex_Hull
     (Diagram : Voronoi_Diagram'Class)
      return Point_Index_Array
   is
      Pts : Geometry.Point_Vectors.Vector;
   begin
      for Site of Diagram.Sites loop
         Pts.Append (Site.Point);
      end loop;

      declare
         Hull : constant Geometry.Point_Vectors.Vector :=
                  Geometry.Convex_Hull (Pts);
      begin
         return Result : Point_Index_Array (1 .. Hull.Last_Index) do
            for I in Result'Range loop
               Result (I) := Pts.Find_Index (Hull.Element (I));
            end loop;
         end return;
      end;
   end Get_Convex_Hull;

   --------------------------
   -- Get_Delauny_Triangle --
   --------------------------

   procedure Get_Delauny_Triangle
     (Diagram : Voronoi_Diagram'Class;
      Index   : Positive;
      A, B, C : out Positive)
   is
      T : constant Voronoi_Triangle := Diagram.Triangles.Element (Index);
   begin
      A := T.A;
      B := T.B;
      C := T.C;
   end Get_Delauny_Triangle;

   ---------------------------------
   -- Get_Delauny_Triangle_Vertex --
   ---------------------------------

   procedure Get_Delauny_Triangle_Vertex
     (Diagram        : Voronoi_Diagram'Class;
      Vertex_Index   : Positive;
      X, Y           : out Real)
   is
   begin
      X := Diagram.Sites.Element (Vertex_Index).Point.X;
      Y := Diagram.Sites.Element (Vertex_Index).Point.Y;
   end Get_Delauny_Triangle_Vertex;

   ---------------------------------
   -- Get_Delauny_Triangle_Vertex --
   ---------------------------------

   procedure Get_Delauny_Triangle_Vertex
     (Diagram        : Voronoi_Diagram'Class;
      Vertex_Index   : Positive;
      X, Y, Z        : out Signed_Unit_Real)
   is
      V_X, V_Y : Real;
   begin
      Diagram.Get_Delauny_Triangle_Vertex (Vertex_Index, V_X, V_Y);

      declare
         Divisor : constant Non_Negative_Real :=
                     1.0 + V_X ** 2 + V_Y ** 2;
      begin
         X := 2.0 * V_X / Divisor;
         Y := 2.0 * V_Y / Divisor;
         Z := (Divisor - 2.0) / Divisor;
      end;

   end Get_Delauny_Triangle_Vertex;

   --------------------------
   -- Get_Spherical_Vertex --
   --------------------------

   procedure Get_Spherical_Vertex
     (Item          : Voronoi_Diagram'Class;
      Vertex_Index  : Positive;
      X, Y, Z       : out Signed_Unit_Real)
   is
      V_X : constant Real := Item.Diagram_Pts.Element (Vertex_Index).X;
      V_Y : constant Real := Item.Diagram_Pts.Element (Vertex_Index).Y;

      Divisor : constant Non_Negative_Real :=
                  1.0 + V_X ** 2 + V_Y ** 2;
   begin
      X := 2.0 * V_X / Divisor;
      Y := 2.0 * V_Y / Divisor;
      Z := (Divisor - 2.0) / Divisor;
   end Get_Spherical_Vertex;

   -------------------
   -- Polygon_Count --
   -------------------

   function Polygon_Count
     (Item : Voronoi_Diagram'Class)
      return Natural
   is
   begin
      return Item.Diagram.Last_Index;
   end Polygon_Count;

   --------------------
   -- Read_Triangles --
   --------------------

   procedure Read_Triangles
     (Path : String;
      Triangles : out Triangle_Vectors.Vector)
   is
      use Ada.Text_IO, Ada.Integer_Text_IO;
      File : File_Type;
   begin

      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            Triangle : Voronoi_Triangle;
            A, B, C  : Natural;
         begin
            Get (File, A);
            Get (File, B);
            Get (File, C);
            Triangle.A := A + 1;
            Triangle.B := B + 1;
            Triangle.C := C + 1;

            Triangles.Append (Triangle);
            Skip_Line (File);
         end;
      end loop;
      Close (File);

   end Read_Triangles;

   -----------------
   -- Sort_Points --
   -----------------

   procedure Sort_Points
     (Diagram  : Voronoi_Diagram'Class;
      Pts      : in out Point_Index_Array;
      Centre   : Voronoi_Point)
   is

      function Less_Than (Index_1, Index_2 : Positive) return Boolean;

      ---------------
      -- Less_Than --
      ---------------

      function Less_Than (Index_1, Index_2 : Positive) return Boolean is
         A : constant Voronoi_Point := Diagram.Diagram_Pts (Index_1);
         B : constant Voronoi_Point := Diagram.Diagram_Pts (Index_2);
         Det : constant Real :=
                 (A.X - Centre.X) * (B.Y - Centre.Y)
               - (B.X - Centre.X) * (A.Y - Centre.Y);
      begin
         return Det < 0.0;
      end Less_Than;

   begin

      for I in 2 .. Pts'Last loop
         declare
            J : Positive := I;
         begin
            while J > Pts'First
              and then Less_Than (Pts (J - 1), Pts (J))
            loop
               declare
                  T : constant Positive := Pts (J - 1);
               begin
                  Pts (J - 1) := Pts (J);
                  Pts (J) := T;
               end;
               J := J - 1;
            end loop;
         end;
      end loop;

   end Sort_Points;

   ------------------
   -- Vertex_Count --
   ------------------

   function Vertex_Count
     (Item          : Voronoi_Diagram'Class;
      Polygon_Index : Positive)
      return Natural
   is
   begin
      return Item.Diagram (Polygon_Index).Count;
   end Vertex_Count;

   --------------
   -- Vertex_X --
   --------------

   function Vertex_X
     (Item : Voronoi_Diagram'Class;
      Polygon_Index : Positive;
      Vertex_Index  : Positive)
      return Real
   is
      Polygon : Voronoi_Polygon renames Item.Diagram (Polygon_Index);
      Point_Index : constant Positive := Polygon.Vertices (Vertex_Index);
   begin
      return Item.Diagram_Pts (Point_Index).X;
   end Vertex_X;

   --------------
   -- Vertex_Y --
   --------------

   function Vertex_Y
     (Item : Voronoi_Diagram'Class;
      Polygon_Index : Positive;
      Vertex_Index  : Positive)
      return Real
   is
      Polygon : Voronoi_Polygon renames Item.Diagram (Polygon_Index);
      Point_Index : constant Positive := Polygon.Vertices (Vertex_Index);
   begin
      return Item.Diagram_Pts (Point_Index).Y;
   end Vertex_Y;

   ------------------
   -- Write_Points --
   ------------------

   procedure Write_Sites
     (Path  : String;
      Sites : Site_Vectors.Vector)
   is
      use Ada.Text_IO, Ada.Float_Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      for Site of Sites loop
         Put (File, Float (Site.Point.X), 1, 6, 0);
         Put (File, " ");
         Put (File, Float (Site.Point.Y), 1, 6, 0);
         New_Line (File);
      end loop;
      Close (File);
   end Write_Sites;

end Hera.Voronoi_Diagrams;
