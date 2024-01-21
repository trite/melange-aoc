let addErrorPrefix = err => {j|Error: $err|j};

let mapWithErrorText = f => Result.fold(addErrorPrefix, f);
