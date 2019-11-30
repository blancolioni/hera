with Hera.Calendar;
with Hera.Random;
with Hera.Real_Images;

with Hera.Updates.Events;

with Hera.Pops.People.Updates;

package body Hera.Pops.People is

   Population_Version : constant Hera.Objects.Object_Version := "0.0.1";

   Log_Pops : constant Boolean := False;

   type Add_Population_Update is
     new Hera.Objects.Root_Update_Type with
      record
         Quantity : Hera.Quantities.Quantity_Type;
      end record;

   overriding procedure Execute
     (Update : Add_Population_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Add_Population_Update)
      return String
   is ("add "
       & Hera.Quantities.Show (Update.Quantity)
       & " population");

   type Remove_Population_Update is
     new Hera.Objects.Root_Update_Type with
      record
         Quantity : Hera.Quantities.Quantity_Type;
      end record;

   overriding procedure Execute
     (Update : Remove_Population_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Remove_Population_Update)
      return String
   is ("remove "
       & Hera.Quantities.Show (Update.Quantity)
       & " population");

   type Happiness_Update is
     new Hera.Objects.Root_Update_Type with
      record
         New_Happiness : Unit_Real;
      end record;

   overriding procedure Execute
     (Update : Happiness_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Happiness_Update)
      return String
   is ("set happiness to "
       & Hera.Real_Images.Approximate_Image (Update.New_Happiness));

   type Population_Update_Type is
     new Hera.Updates.Update_Interface with
      record
         Pop : Population_Type;
      end record;

   overriding procedure Activate
     (Update : Population_Update_Type);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Population_Update_Type)
   is
      use Hera.Quantities;
   begin
      if Update.Pop.Size > Zero then
         Update.Pop.Log
           ("activating pop "
            & Update.Pop.Class.Tag
            & "; size "
            & Hera.Quantities.Show (Update.Pop.Size)
            & " cash " & Hera.Money.Show (Update.Pop.Cash)
            & " happiness"
            & Natural'Image (Natural (Update.Pop.Happiness * 100.0)));

         Hera.Pops.People.Updates.Buy_Commodities (Update.Pop);
         Hera.Pops.People.Updates.Consume_Commodities (Update.Pop);

      end if;

      Hera.Updates.Events.Update_Next_Cycle (Update);
   end Activate;

   --------------------
   -- Add_Population --
   --------------------

   procedure Add_Population
     (Pop      : Root_Population_Object'Class;
      Quantity : Hera.Quantities.Quantity_Type)
   is
   begin
      Pop.Add_Update
        (Add_Population_Update'
           (Hera.Objects.Root_Update_Type with
            Quantity => Quantity));
   end Add_Population;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Add_Population_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class)
   is
      use Hera.Quantities;
      Pop : Root_Population_Object'Class renames
              Root_Population_Object'Class (Object);
   begin
      Pop.Size := Pop.Size + Update.Quantity;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Remove_Population_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class)
   is
      use Hera.Quantities;
      Pop : Root_Population_Object'Class renames
        Root_Population_Object'Class (Object);
   begin
      pragma Assert (Pop.Size >= Update.Quantity);
      Pop.Size := Pop.Size - Update.Quantity;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Happiness_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class)
   is
   begin
      Root_Population_Object (Object).Happiness := Update.New_Happiness;
   end Execute;

   ---------------
   -- Get_Stock --
   ---------------

   overriding function Get_Stock
     (From      : Root_Population_Object;
      Commodity : Hera.Commodities.Commodity_Type)
      return Hera.Commodities.Stock_Entry
   is
   begin
      return From.Stock.Get_Stock (Commodity);
   end Get_Stock;

   ---------------------
   -- Move_Population --
   ---------------------

   procedure Move_Population
     (From        : Root_Population_Object'Class;
      To          : not null access constant Root_Population_Object'Class;
      Transaction : not null access constant
        Hera.Accounts.Root_Transaction_Type'Class;
      Quantity    : Hera.Quantities.Quantity_Type)
   is
      Proportion : constant Unit_Real :=
        Hera.Quantities.To_Real (Quantity)
        / Hera.Quantities.To_Real (From.Size);
   begin
      From.Account.Move_Cash (To.Account, Transaction, Proportion);
      From.Remove_Population (Quantity);
      To.Add_Population (Quantity);
   end Move_Population;

   --------------------
   -- New_Population --
   --------------------

   function New_Population
     (Class  : Hera.Pops.Classes.Pop_Class_Type;
      Size   : Hera.Quantities.Quantity_Type;
      Cash   : Hera.Money.Money_Type;
      Colony : not null access constant Hera.Colonies.Root_Colony_Type'Class;
      Market : Hera.Markets.Market_Type)
      return Population_Type
   is
      use Hera.Calendar;
      Pop : Root_Population_Object := Root_Population_Object'
        (Hera.Objects.Root_Hera_Object with
         Class     => Class,
         Size      => Size,
         Market    => Market,
         Colony    => Pop_Colony (Colony),
         Stock     => <>,
         Happiness => 1.0,
         Account   =>
           Hera.Accounts.New_Account
             (Class.Tag, Cash));
   begin
      Pop.Initialize (Population_Version);
      Pop.Stock.Initialize_Stock_List (Pop.Identifier);

      return New_Pop : constant Population_Type :=
        Population_Type (Pop.Save_Object)
      do
         Hera.Updates.Events.Update_At
           (Hera.Calendar.Clock
            + Days (Hera.Random.Unit_Random * Hera.Updates.Update_Cycle_Days),
            Population_Update_Type'
              (Pop => New_Pop));
      end return;
   end New_Population;

   ------------
   -- On_Buy --
   ------------

   overriding procedure On_Buy
     (Pop         : Root_Population_Object;
      Commodity   : Hera.Commodities.Commodity_Type;
      Quantity    : Hera.Quantities.Quantity_Type;
      Total_Paid  : Hera.Money.Money_Type)
   is
   begin
      if Log_Pops then
         Pop.Log ("bought " & Hera.Quantities.Show (Quantity)
                  & " " & Commodity.Tag & " for "
                  & Hera.Money.Show (Total_Paid));
      end if;
   end On_Buy;

   overriding procedure On_Sell
     (Pop          : Root_Population_Object;
      Commodity    : Hera.Commodities.Commodity_Type;
      Quantity     : Hera.Quantities.Quantity_Type;
      Total_Earned : Hera.Money.Money_Type)
   is null;

   -----------------------
   -- Remove_Population --
   -----------------------

   procedure Remove_Population
     (Pop      : Root_Population_Object'Class;
      Quantity : Hera.Quantities.Quantity_Type)
   is
   begin
      Pop.Add_Update
        (Remove_Population_Update'
           (Hera.Objects.Root_Update_Type with
                Quantity => Quantity));
   end Remove_Population;

   ----------------
   -- Scan_Stock --
   ----------------

   overriding procedure Scan_Stock
     (List      : Root_Population_Object;
      Process   : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Stock : Hera.Commodities.Stock_Entry))
   is
   begin
      List.Stock.Scan_Stock (Process);
   end Scan_Stock;

   ---------------
   -- Set_Stock --
   ---------------

   overriding procedure Set_Stock
     (To        : Root_Population_Object;
      Commodity : Hera.Commodities.Commodity_Type;
      Stock     : Hera.Commodities.Stock_Entry)
   is
   begin
      To.Stock.Set_Stock (Commodity, Stock);
   end Set_Stock;

   ----------------------
   -- Update_Happiness --
   ----------------------

   procedure Update_Happiness
     (Pop           : Root_Population_Object'Class;
      New_Happiness : Unit_Real)
   is
   begin
      Pop.Add_Update
        (Happiness_Update'
           (Hera.Objects.Root_Update_Type with
                New_Happiness => New_Happiness));
   end Update_Happiness;

   ------------------
   -- Update_Stock --
   ------------------

   overriding procedure Update_Stock
     (Pop : in out Root_Population_Object;
      Commodity : Hera.Commodities.Commodity_Type;
      Stock     : Hera.Commodities.Stock_Entry)
   is
   begin
      Pop.Stock.Update_Stock (Commodity, Stock);
   end Update_Stock;

end Hera.Pops.People;
