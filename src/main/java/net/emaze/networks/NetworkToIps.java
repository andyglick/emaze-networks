package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.tuples.Pair;

public class NetworkToIps implements Delegate<Pair<Ip, Ip>, Network> {

    @Override
    public Pair<Ip, Ip> perform(Network cidr) {
        dbc.precondition(cidr != null, "cidr cannot be null");
        return Pair.of(cidr.firstIp(), cidr.lastIp());
    }
}
