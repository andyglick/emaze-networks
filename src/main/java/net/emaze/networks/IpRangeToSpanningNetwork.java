package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.order.Order;

public class IpRangeToSpanningNetwork implements BinaryDelegate<Network, Ip, Ip> {

    @Override
    public Network perform(Ip firstIp, Ip lastIp) {
        dbc.precondition(firstIp != null, "firstIp cannot be null");
        dbc.precondition(lastIp != null, "lastIp cannot be null");
        dbc.precondition(Order.of(firstIp.compareTo(lastIp)) != Order.GT, "lastIp cannot be lesser than firstIp");
        dbc.precondition(firstIp.version().equals(lastIp.version()), "firstIp and lastIp must be represented with the same IPvX version");
        Mask netmask = null;
        Network candidate;
        do {
            netmask = (netmask == null ? firstIp.version().getNarrowestMask() : netmask.widenHosts());
            candidate = Network.byContainedIp(lastIp, netmask);
        } while (!candidate.contains(firstIp));
        return candidate;
    }
}
