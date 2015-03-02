                
#include "leveldb/db.h"
namespace leveldb {

class KeyValuePair : public Slice {
    public:
        /* Create an empty KeyValuePair. */
        KeyValuePair()
        : Slice (), tag_(-1), value_(""), value_size_(0) { }
        /* Create a KeyValuePair */
        KeyValuePair (int tag, const char* d, size_t n,
                      const char* v, size_t vs)
        : Slice (d, n), tag_(tag), value_(v), value_size_(vs) { }
        //~ KeyValuePair ();

        /* Return the integer value of tag */
        const int tag() const { return tag_; }
        /* Return a pointer to the referenced key */
        const char* key() const { return data(); }
        /* Return the length (in bytes) of the referenced key */
        size_t key_size() const { return size(); }
        /* Return a pointer to the referenced value */
        const char* value() const { return value_; }
        /* Return the length (in bytes) of the referenced value */
        size_t value_size() const { return value_size_; }
        /*Compare function to be used by STL: Algorithm*/
        bool operator()(const KeyValuePair& a, const KeyValuePair& b) const {
            if ( a.compare(b) < 0){
                return true;
            }
            else {
                return false;
            }
        }
    private:
        int tag_;
        const char* value_;
        size_t value_size_;
};
}
