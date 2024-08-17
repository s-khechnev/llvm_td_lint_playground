
  $ cat > 1.json <<EOF
  > { "k1": {
  >          "!superclasses": [ "Instruction", "Class2" ],
  >          "field": "k1"
  >         },
  >   "k2": {
  >          "!superclasses": [ "Not_instruction", "Class3" ],
  >          "field": "k2"
  >         },
  >   "k3": {
  >          "xxxxx": [ "Not_instruction", "Class3" ],
  >          "field": "k2"
  >         },
  >    "k4int": 18
  > }
  > EOF
  $ cat 1.json | nl -ba
       1	{ "k1": {
       2	         "!superclasses": [ "Instruction", "Class2" ],
       3	         "field": "k1"
       4	        },
       5	  "k2": {
       6	         "!superclasses": [ "Not_instruction", "Class3" ],
       7	         "field": "k2"
       8	        },
       9	  "k3": {
      10	         "xxxxx": [ "Not_instruction", "Class3" ],
      11	         "field": "k2"
      12	        },
      13	   "k4int": 18
      14	}
  $ jq -c '. ' 1.json
  {"k1":{"!superclasses":["Instruction","Class2"],"field":"k1"},"k2":{"!superclasses":["Not_instruction","Class3"],"field":"k2"},"k3":{"xxxxx":["Not_instruction","Class3"],"field":"k2"},"k4int":18}

  $ jq -c '.[]  | select(."!superclasses" != null)' 1.json
  jq: error (at 1.json:14): Cannot index number with string "!superclasses"
  {"!superclasses":["Instruction","Class2"],"field":"k1"}
  {"!superclasses":["Not_instruction","Class3"],"field":"k2"}
  [5]
  $ jq -c '. | with_entries( select (.value| type == "object")   | select(.value."!superclasses" != null)  | select (  .value."!superclasses"[] | contains("Instruction") ) )' 1.json
  {"k1":{"!superclasses":["Instruction","Class2"],"field":"k1"}}
  $ jq -c '. | with_entries( select (.value| type == "object")   | select(.value."!superclasses" != null)  | select ( .value."!superclasses" | type == "array" ) ) | .' 1.json | jq .
  {
    "k1": {
      "!superclasses": [
        "Instruction",
        "Class2"
      ],
      "field": "k1"
    },
    "k2": {
      "!superclasses": [
        "Not_instruction",
        "Class3"
      ],
      "field": "k2"
    }
  }

