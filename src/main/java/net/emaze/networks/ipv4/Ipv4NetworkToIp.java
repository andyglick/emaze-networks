package net.emaze.networks.ipv4;

import java.util.function.Function;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv4NetworkToIp implements Function<Ipv4Network, Pair<Ipv4, Ipv4>> {

    @Override
    public Pair<Ipv4, Ipv4> apply(Ipv4Network cidr) {
        dbc.precondition(cidr != null, "cidr cannot be null");
        return Pair.of(cidr.firstIp(), cidr.lastIp());
    }
}
