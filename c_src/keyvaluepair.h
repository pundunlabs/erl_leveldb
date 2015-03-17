
#include "leveldb/db.h"
#include "erl_nif.h"

namespace leveldb {

class KeyValuePair : public Slice {
    public:
        /* Create an empty KeyValuePair. */
        KeyValuePair()
	: Slice (), tag_(-1) { }
        /* Create a KeyValuePair */
        KeyValuePair (int tag, const char* d, size_t n,
                      ErlNifBinary v)
        : Slice (d, n), tag_(tag), value_(v) { }
        //~ KeyValuePair ();
        /* Return the integer value of tag */
        const int tag() const { return tag_; }
        /* Return a pointer to the referenced key */
        const char* key() const { return data(); }
        /* Return the length (in bytes) of the referenced key */
        size_t key_size() const { return size(); }
        /* Return the referenced binary value */
        ErlNifBinary value() const { return value_; }
        /*Compare function to be used by STL: Algorithm */
        bool operator()(const KeyValuePair& a, const KeyValuePair& b) const {
            if ( a.compare(b) < 0){
                return true;
            }
            else {
                return false;
            }
        }
    private:
        signed char tag_;
	ErlNifBinary value_;
};
}
