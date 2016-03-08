package net.emaze.networks.ipv6;

import java.util.function.Function;
import net.emaze.dysfunctional.contracts.dbc;

public class Ipv6PreviousIp implements Function<Ipv6, Ipv6> {

    @Override
    public Ipv6 apply(Ipv6 ip) {
        dbc.precondition(ip != null, "ip cannot be null");
        return ip.previous();
    }
}
