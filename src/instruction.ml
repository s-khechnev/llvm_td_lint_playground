type t = {
  mnemonic : string;
  operands : string list;
  ins : string list;
  outs : string list;
  mayLoad : bool;
  mayStore : bool;
}
