
package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.ranges.DenseRange;


public class IpRangeToSpanningCidr implements Delegate<Cidr, DenseRange<Ipv4>> {

    @Override
    public Cidr perform(DenseRange<Ipv4> range) {
        dbc.precondition(range != null, "range cannot be null");
        dbc.precondition(range.end().hasValue(), "range cannot be open-ended");
        int prefix = 32;
        Cidr candidate;
        do {
            candidate = new Cidr(range.end().value().previous(), Netmask.fromBits(prefix--)); //FIXME: range problem with end element
        } while (!candidate.contains(range.begin()));
        return candidate;
    }

}
