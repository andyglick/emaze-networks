package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.tuples.Pair;

public class CidrToIps implements Delegate<Pair<Ipv4, Ipv4>, Cidr> {

    @Override
    public Pair<Ipv4, Ipv4> perform(Cidr cidr) {
        dbc.precondition(cidr != null, "cidr cannot be null");
        return Pair.of(cidr.firstIp(), cidr.lastIp());
    }
}