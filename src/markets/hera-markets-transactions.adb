package body Hera.Markets.Transactions is

   type Market_Transaction_Type is
     new Hera.Accounts.Root_Transaction_Type with
      record
         Commodity : Hera.Commodities.Commodity_Type;
         Quantity  : Hera.Quantities.Quantity_Type;
         Price     : Hera.Money.Price_Type;
      end record;

   overriding function Class_Name
     (Transaction : Market_Transaction_Type)
      return String
   is ("commodity");

   overriding function Description
     (Transaction : Market_Transaction_Type)
      return String
   is (Hera.Money.Show (Transaction.Amount)
       & " for " & Hera.Quantities.Show (Transaction.Quantity)
       & " " & Transaction.Commodity.Tag
       & " " & Hera.Money.Show (Transaction.Price)
       & " ea;");

   ------------------------
   -- Market_Transaction --
   ------------------------

   function Market_Transaction
     (Buyer     : not null access constant Trader_Interface'Class;
      Seller    : not null access constant Trader_Interface'Class;
      Commodity : Hera.Commodities.Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Price     : Hera.Money.Price_Type)
      return Hera.Accounts.Transaction_Type
   is
      Result : Market_Transaction_Type :=
        Market_Transaction_Type'
          (Hera.Accounts.Root_Transaction_Type with
           Commodity => Commodity,
           Quantity  => Quantity,
           Price     => Price);
   begin
      Result.New_Transaction
        (Buyer.Account, Seller.Account, Hera.Money.Total (Price, Quantity));
      return new Market_Transaction_Type'(Result);
   end Market_Transaction;

end Hera.Markets.Transactions;
