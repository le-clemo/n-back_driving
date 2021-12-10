#set_similarities = "(1 1 0) (1 2 -0.1) (1 3 -0.2) (1 4 -0.4) (1 5 -0.8)"  
similarities = ["-0.5", "-1", "-1.5", "-2"]

set_similarities = "(1 1 0)"
for i in range(1,5):
	string = " ".join(['(', '1', str(1+i), similarities[i-1], ')'])
	set_similarities = " ".join([set_similarities, string])

for i in range(2, 50):
	sim_list = similarities.copy()
	for j in range(1, 5):
		string = " ".join(['(', str(i), str(i+j), sim_list.pop(0), ')'])
		set_similarities = " ".join([set_similarities, string])

for i in range(1, 100):
	string = " ".join(['(', "0", str(i), "-100", ')'])
	set_similarities = " ".join([set_similarities, string])

set_similarities = " ".join([set_similarities, ')'])



print(set_similarities)


