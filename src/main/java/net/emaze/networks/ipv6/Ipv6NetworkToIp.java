package net.emaze.networks.ipv6;

import java.util.function.Function;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv6NetworkToIp implements Function<Ipv6Network, Pair<Ipv6, Ipv6>> {

    @Override
    public Pair<Ipv6, Ipv6> apply(Ipv6Network cidr) {
        dbc.precondition(cidr != null, "cidr cannot be null");
        return Pair.of(cidr.firstIp(), cidr.lastIp());
    }
}
