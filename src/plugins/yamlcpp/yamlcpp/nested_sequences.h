// clang-format off

#define PREFIX "user/examples/yamlcpp/"

ksNew (20,
       keyNew (PREFIX "American Football", KEY_META, "array", "#2", KEY_END),
       keyNew (PREFIX "American Football/#0", KEY_VALUE, "American Football (EP)", KEY_END),
       keyNew (PREFIX "American Football/#1", KEY_VALUE, "American Football (LP)", KEY_END),
       keyNew (PREFIX "American Football/#2/American Football (LP2)", KEY_META, "array", "#1", KEY_END),
       keyNew (PREFIX "American Football/#2/American Football (LP2)/#0", KEY_VALUE, "Where Are We Now?", KEY_END),
       keyNew (PREFIX "American Football/#2/American Football (LP2)/#1", KEY_VALUE, "My Instincts Are The Enemy", KEY_END),
       keyNew (PREFIX "Napalm Death/Apex Predator-Easy Meat", KEY_META, "array", "#2", KEY_END),
       keyNew (PREFIX "Napalm Death/Apex Predator-Easy Meat/#0", KEY_VALUE, "Smash A Single Digit" , KEY_END),
       keyNew (PREFIX "Napalm Death/Apex Predator-Easy Meat/#1", KEY_VALUE, "How The Years Condemn" , KEY_END),
       keyNew (PREFIX "Napalm Death/Apex Predator-Easy Meat/#2", KEY_VALUE, "Dear Slum Landlord..." , KEY_END),
       KS_END)
