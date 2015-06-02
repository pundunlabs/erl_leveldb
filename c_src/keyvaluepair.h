
#include "leveldb/db.h"
#include "erl_nif.h"

namespace leveldb {

class KeyValuePair : public Slice {
    public:
        /* Create an empty KeyValuePair. */
        KeyValuePair()
	: Slice (), tag_(-1) { }
        /* Create an KeyValuePair for comp function. */
        KeyValuePair(bool a)
	: Slice (), tag_(-1), ascending_(a) { }
        /* Create a KeyValuePair */
        KeyValuePair (int tag, const char* d, size_t n,
                      ErlNifBinary v)
        : Slice (d, n), tag_(tag), value_(v) { }
        //~ KeyValuePair ();
        /* Return the integer value of tag */
        const int tag() const { return tag_; }
        /* Return the bool value of ascending */
        const bool ascending() const { return ascending_; }
        /* Return a pointer to the referenced key */
        const char* key() const { return data(); }
        /* Return the length (in bytes) of the referenced key */
        size_t key_size() const { return size(); }
        /* Return the referenced binary value */
        ErlNifBinary value() const { return value_; }
        /*Compare function to be used by STL: Algorithm */
        bool operator()(const KeyValuePair& a, const KeyValuePair& b) const {
            if (ascending())
		return a.compare(b) < 0;
	    else
		return a.compare(b) > 0;
        }
    private:
        unsigned short int tag_;
	bool ascending_;
	ErlNifBinary value_;
};
}
