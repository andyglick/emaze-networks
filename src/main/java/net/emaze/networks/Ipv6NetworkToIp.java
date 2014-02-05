package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv6NetworkToIp implements Delegate<Pair<Ipv6, Ipv6>, Ipv6Network> {

    @Override
    public Pair<Ipv6, Ipv6> perform(Ipv6Network cidr) {
        dbc.precondition(cidr != null, "cidr cannot be null");
        return Pair.of(cidr.firstIp(), cidr.lastIp());
    }
}
