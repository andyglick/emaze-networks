package net.emaze.networks.ipv4;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv4NetworkToIp implements Delegate<Pair<Ipv4, Ipv4>, Ipv4Network> {

    @Override
    public Pair<Ipv4, Ipv4> perform(Ipv4Network cidr) {
        dbc.precondition(cidr != null, "cidr cannot be null");
        return Pair.of(cidr.firstIp(), cidr.lastIp());
    }
}
