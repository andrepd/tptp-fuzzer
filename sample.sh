./tptp-fuzzer compare-incremental \
	"iprover:/home/andrepd/Development/iprover/iproveropt" \
	"vprover:../vampire/vampire4.2.2_noz3" \
	--num-clauses 100 \
	--num-literals-per-clause 1--4 \
	--num-vars 3 \
	--ratio-vars 0.315 \
	--num-funcs 3 \
	--funcs-arity 0--3 \
	--num-preds 3 \
	--preds-arity 1--3 \
	--max-depth 3 \
	# --putas \
	# --conas \
	
