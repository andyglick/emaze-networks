package net.emaze.networks.ipv6;

import java.util.function.BiFunction;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.order.Order;

public class Ipv6RangeToSpanningNetwork implements BiFunction<Ipv6, Ipv6, Ipv6Network> {

    @Override
    public Ipv6Network apply(Ipv6 firstIp, Ipv6 lastIp) {
        dbc.precondition(firstIp != null, "firstIp cannot be null");
        dbc.precondition(lastIp != null, "lastIp cannot be null");
        dbc.precondition(Order.of(firstIp.compareTo(lastIp)) != Order.GT, "lastIp cannot be lesser than firstIp");
        Ipv6Mask netmask = null;
        Ipv6Network candidate;
        do {
            netmask = (netmask == null ? Ipv6Mask.getNarrowestMask() : netmask.widenHosts());
            candidate = Ipv6Network.byContainedIp(lastIp, netmask);
        } while (!candidate.contains(firstIp));
        return candidate;
    }
}
