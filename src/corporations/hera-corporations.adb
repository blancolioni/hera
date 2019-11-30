with Hera.Identifiers;

with Hera.Colonies;
with Hera.Star_Systems;

package body Hera.Corporations is

   Corporation_Version : constant Hera.Objects.Object_Version := "0.0.1";

   package Corporation_Vectors is
     new Ada.Containers.Vectors (Positive, Corporation_Type);

   Vector : Corporation_Vectors.Vector;

   type Add_Colony_Update is
     new Hera.Objects.Root_Update_Type with
      record
         Colony : Corporate_Colony;
      end record;

   overriding procedure Execute
     (Update : Add_Colony_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Add_Colony_Update)
      return String
   is ("add presence in colony " & Update.Colony.Name);

   type Update_Knowledge_Update is
     new Hera.Objects.Root_Update_Type with
      record
         Object : Hera.Objects.Hera_Object;
         Level  : Hera.Knowledge.Knowledge_Level_Type;
      end record;

   overriding procedure Execute
     (Update : Update_Knowledge_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Update_Knowledge_Update)
      return String
   is ("change knowledge of " & Update.Object.Log_Id & " to "
       & Update.Level'Image);

   ----------------
   -- Add_Colony --
   ----------------

   procedure Add_Colony
     (Corporation : Root_Corporation_Type'Class;
      Colony      : not null access constant
        Hera.Colonies.Root_Colony_Type'Class)
   is
   begin
      Corporation.Add_Update
        (Add_Colony_Update'
           (Hera.Objects.Root_Update_Type with
                Colony => Corporate_Colony (Colony)));
   end Add_Colony;

   --------------
   -- Colonies --
   --------------

   function Colonies
     (Corporation : Root_Corporation_Type'Class)
      return Corporate_Colony_Array
   is
      use type Hera.Identifiers.Object_Identifier;
      Result : Corporate_Colony_Array (1 .. Corporation.Colonies.Last_Index);
      Count  : Natural := 0;
   begin
      for Colony of Corporation.Colonies loop
         if Colony.Government.Identifier = Corporation.Identifier then
            Count := Count + 1;
            Result (Count) := Colony;
         end if;
      end loop;
      return Result (1 .. Count);
   end Colonies;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Add_Colony_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class)
   is
   begin
      Root_Corporation_Type (Object).Colonies.Append (Update.Colony);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Update_Knowledge_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class)
   is
   begin
      Root_Corporation_Type (Object).Knowledge.Update_Knowledge
        (Update.Object, Update.Level);
   end Execute;

   ------------
   -- Exists --
   ------------

   function Exists
     (Name : String)
      return Boolean
   is
   begin
      for Item of Vector loop
         if Item.Name = Name then
            return True;
         end if;
      end loop;
      return False;
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Name : String)
      return Corporation_Type
   is
   begin
      for Item of Vector loop
         if Item.Name = Name then
            return Item;
         end if;
      end loop;
      raise Constraint_Error with
        "no such corporation: " & Name;
   end Get;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Process : not null access
        procedure (Corporation : Corporation_Type))
   is
   begin
      for Corporation of Vector loop
         Process (Corporation);
      end loop;
   end Iterate;

   ---------------------
   -- Knowledge_Level --
   ---------------------

   overriding function Knowledge_Level
     (Corporation : Root_Corporation_Type;
      Object      : Hera.Knowledge.Knowable_Interface'Class)
      return Hera.Knowledge.Knowledge_Level_Type
   is
   begin
      return Corporation.Knowledge.Knowledge_Level
        (Hera.Objects.Root_Hera_Object'Class (Object));
   end Knowledge_Level;

   ---------------------
   -- New_Corporation --
   ---------------------

   function New_Corporation
     (Name       : String;
      Cash       : Hera.Money.Money_Type;
      Planet     : Hera.Planets.Planet_Type;
      Autoplayer : Boolean)
      return Corporation_Type
   is

      function Initial_Knowledge
        return Hera.Objects.Knowledge.Object_Knowledge_Type;

      -----------------------
      -- Initial_Knowledge --
      -----------------------

      function Initial_Knowledge
        return Hera.Objects.Knowledge.Object_Knowledge_Type
      is
         Result : Hera.Objects.Knowledge.Object_Knowledge_Type;

         procedure Add_Entity_Knowledge
           (Entity : Hera.Star_Systems.Star_System_Entity);

         procedure Add_System_Knowledge
           (System : Hera.Star_Systems.Star_System_Type);

         --------------------------
         -- Add_Entity_Knowledge --
         --------------------------

         procedure Add_Entity_Knowledge
           (Entity : Hera.Star_Systems.Star_System_Entity)
         is
            use type Hera.Star_Systems.Star_System_Entity;
         begin
            if Hera.Star_Systems.Star_System_Entity (Planet) = Entity then
               Result.Update_Knowledge (Entity, Hera.Knowledge.Full);
            else
               Result.Update_Knowledge (Entity, Hera.Knowledge.Exists);
            end if;
         end Add_Entity_Knowledge;

         --------------------------
         -- Add_System_Knowledge --
         --------------------------

         procedure Add_System_Knowledge
           (System : Hera.Star_Systems.Star_System_Type)
         is
            use type Hera.Star_Systems.Star_System_Type;
         begin
            if System = Planet.System then
               Result.Update_Knowledge (System, Hera.Knowledge.Full);
            elsif Hera.Star_Systems.Distance (System, Planet.System)
              < 20.0
            then
               Result.Update_Knowledge (System, Hera.Knowledge.Exists);
            end if;
         end Add_System_Knowledge;

      begin
         Planet.System.Iterate_Entities (Add_Entity_Knowledge'Access);
         Hera.Star_Systems.Iterate (Add_System_Knowledge'Access);
         return Result;
      end Initial_Knowledge;

      Corp_Rec : constant Root_Corporation_Type :=
        Root_Corporation_Type'
          (Hera.Objects.Root_Named_Object with
           Account     => Hera.Accounts.New_Account (Name, Cash),
           Home_Planet => Planet,
           Autoplayer  => Autoplayer,
           Colonies    => Corporate_Colony_Vectors.Empty_Vector,
           Knowledge   => Initial_Knowledge);

      Corp : constant Corporation_Type :=
               Corporation_Type
                     (Corp_Rec.New_Named_Object
                        (Corporation_Version, Name));
   begin
      Vector.Append (Corp);
      return Corp;
   end New_Corporation;

   ------------
   -- On_Buy --
   ------------

   overriding procedure On_Buy
     (Corporation : Root_Corporation_Type;
      Commodity   : Hera.Commodities.Commodity_Type;
      Quantity    : Hera.Quantities.Quantity_Type;
      Total_Paid  : Hera.Money.Money_Type)
   is
   begin
      null;
   end On_Buy;

   -------------
   -- On_Sell --
   -------------

   overriding procedure On_Sell
     (Corporation  : Root_Corporation_Type;
      Commodity    : Hera.Commodities.Commodity_Type;
      Quantity     : Hera.Quantities.Quantity_Type;
      Total_Earned : Hera.Money.Money_Type)
   is
   begin
      null;
   end On_Sell;

   ----------
   -- Save --
   ----------

   procedure Save (Config : in out Tropos.Configuration) is
      Corp_Config : Tropos.Configuration :=
        Tropos.New_Config ("corporations");
   begin
      for I in 1 .. Vector.Last_Index loop
         declare
            Item : Tropos.Configuration := Tropos.New_Config (I);
         begin
            Vector.Element (I).Save (Item);
            Corp_Config.Add (Item);
         end;
      end loop;
      Config.Add (Corp_Config);
   end Save;

   ----------------------
   -- Update_Knowledge --
   ----------------------

   procedure Update_Knowledge
     (Corporation     : Root_Corporation_Type'Class;
      Object          : not null access constant
        Hera.Objects.Root_Hera_Object'Class;
      Knowledge_Level : Hera.Knowledge.Knowledge_Level_Type)
   is
   begin
      Corporation.Add_Update
        (Update_Knowledge_Update'
           (Hera.Objects.Root_Update_Type with
                Object => Hera.Objects.Hera_Object (Object),
            Level  => Knowledge_Level));
   end Update_Knowledge;

end Hera.Corporations;
