package body Hera.Sectors is

   type Remove_Resources is
     new Hera.Objects.Root_Update_Type with
      record
         Resource : Hera.Commodities.Commodity_Type;
         Quantity : Hera.Quantities.Quantity_Type;
      end record;

   overriding procedure Execute
     (Update : Remove_Resources;
      Target : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Remove_Resources)
      return String
   is ("remove "
       & Hera.Quantities.Show (Update.Quantity)
       & " " & Update.Resource.Tag);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Remove_Resources;
      Target : in out Hera.Objects.Root_Hera_Object'Class)
   is
      use type Hera.Commodities.Commodity_Type;
      use type Hera.Quantities.Quantity_Type;
      Sector : Root_Sector_Type'Class renames
        Root_Sector_Type'Class (Target);
   begin
      for Deposit of Sector.Deposits loop
         if Deposit.Resource = Update.Resource then
            if Deposit.Quantity < Update.Quantity then
               raise Constraint_Error with
                 "cannot remove " & Hera.Quantities.Show (Update.Quantity)
                 & " " & Update.Resource.Tag
                 & " from sector " & String (Sector.Identifier)
                 & " because there are only "
                 & Hera.Quantities.Show (Deposit.Quantity);
            end if;
            Deposit.Quantity := Deposit.Quantity - Update.Quantity;
            Sector.Log
              ("remove " & Hera.Quantities.Show (Update.Quantity)
               & " from " & Deposit.Resource.Tag
               & " deposit; "
               & Hera.Quantities.Show (Deposit.Quantity) & " remaining");
            return;
         end if;
      end loop;
      raise Constraint_Error with
        "cannot remove " & Hera.Quantities.Show (Update.Quantity)
        & " " & Update.Resource.Tag
        & " from sector " & String (Sector.Identifier)
        & " because there are none";
   end Execute;

   --------------------
   -- Mine_Resources --
   --------------------

   procedure Mine_Resources
     (Sector   : Root_Sector_Type'Class;
      Resource : Hera.Commodities.Resource_Type;
      Quantity : Hera.Quantities.Quantity_Type)
   is
   begin
      Hera.Objects.Add_Update
        (Target => Sector,
         Update => Remove_Resources'
           (Hera.Objects.Root_Update_Type with
                Resource => Resource, Quantity => Quantity));
   end Mine_Resources;

   ---------------
   -- Remaining --
   ---------------

   function Remaining
     (Sector   : Root_Sector_Type'Class;
      Resource : Hera.Commodities.Commodity_Type)
      return Hera.Quantities.Quantity_Type
   is
      use type Hera.Commodities.Commodity_Type;
   begin
      for Deposit of Sector.Deposits loop
         if Deposit.Resource = Resource then
            return Deposit.Quantity;
         end if;
      end loop;
      return Hera.Quantities.Zero;
   end Remaining;

   ---------------
   -- Resources --
   ---------------

   function Resources
     (Sector : Root_Sector_Type'Class)
      return Hera.Commodities.Commodity_Array
   is
      Result : Hera.Commodities.Commodity_Array
        (1 .. Natural (Sector.Deposits.Length));
      Count  : Natural := 0;
   begin
      for Deposit of Sector.Deposits loop
         Count := Count + 1;
         Result (Count) := Deposit.Resource;
      end loop;
      return Result;
   end Resources;

   -----------
   -- Yield --
   -----------

   function Yield
     (Sector   : Root_Sector_Type'Class;
      Resource : Hera.Commodities.Commodity_Type)
      return Hera.Quantities.Quantity_Type
   is
      use type Hera.Commodities.Commodity_Type;
   begin
      for Deposit of Sector.Deposits loop
         if Deposit.Resource = Resource then
            return Hera.Quantities.Min (Deposit.Yield, Deposit.Quantity);
         end if;
      end loop;
      return Hera.Quantities.Zero;
   end Yield;

end Hera.Sectors;
