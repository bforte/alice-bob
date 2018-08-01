# Alice & Bob

Alice & Bob is a minimalist esolang based on Brain-Flak: It replaces
the left stack and right stack by Alice's stack that behaves as usual
and Bob's queue.

Since Bob is a bit different he interprets 0 as truthy and anything else as
falsy, thus `{code}` will loop while 0 is the first element in the queue.
