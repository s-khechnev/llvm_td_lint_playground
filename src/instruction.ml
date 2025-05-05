type t = {
  mnemonic : string;
  operands : string list;
  ins : string list;
  outs : string list;
  mayLoad : bool;
  mayStore : bool;
  ins_csr : string list;
  outs_csr : string list;
}
