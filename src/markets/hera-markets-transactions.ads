package Hera.Markets.Transactions is

   function Market_Transaction
     (Buyer     : not null access constant Trader_Interface'Class;
      Seller    : not null access constant Trader_Interface'Class;
      Commodity : Hera.Commodities.Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Price     : Hera.Money.Price_Type)
      return Hera.Accounts.Transaction_Type;

end Hera.Markets.Transactions;
