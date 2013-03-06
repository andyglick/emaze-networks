
package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;


public class BitsToLong implements Delegate<Long, Integer> {

    @Override
    public Long perform(Integer bits) {
        dbc.precondition(bits != null, "bits cannot be null");
        dbc.precondition((bits > 0) && (bits <= 32), "bits must be between 1 and 32");
        return ((1 << 31) >> (bits - 1)) & 0xFFFFFFFFL;
    }

}
