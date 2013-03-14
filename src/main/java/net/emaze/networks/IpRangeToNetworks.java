package net.emaze.networks;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.Multiplexing;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.order.Order;

public class IpRangeToNetworks implements BinaryDelegate<List<Network>, Ip, Ip> {

    private static final SortNetworksByFirstThenLastIp sorter = new SortNetworksByFirstThenLastIp();

    @Override
    public List<Network> perform(Ip firstIp, Ip lastIp) {
        dbc.precondition(firstIp != null, "firstIp cannot be null");
        dbc.precondition(lastIp != null, "lastIp cannot be null");
        dbc.precondition(Order.of(firstIp.compareTo(lastIp)) != Order.GT, "lastIp cannot be lesser than firstIp");
        // Obtain a spanning cidr containing entire range
        final Network spanningCidr = new IpRangeToSpanningNetwork().perform(firstIp, lastIp);
        if (spanningCidr.firstIp().equals(firstIp) && spanningCidr.lastIp().equals(lastIp)) {
            // Spanning CIDR matches start and end exactly;
            return Collections.singletonList(spanningCidr);
        }
        if (spanningCidr.lastIp().equals(lastIp)) {
            // Spanning CIDR matches range end exactly;
            return pruneSpanningCidrHead(spanningCidr, firstIp);
        }
        if (spanningCidr.firstIp().equals(firstIp)) {
            // Spanning CIDR matches range start exactly
            return pruneSpanningCidrTail(spanningCidr, lastIp);
        }
        // Spanning CIDR overlaps entire range
        final LinkedList<Network> prunedHead = pruneSpanningCidrHead(spanningCidr, firstIp);
        final List<Network> prunedTail = pruneSpanningCidrTail(prunedHead.removeLast(), lastIp);
        return Consumers.all(Multiplexing.flatten(prunedHead, prunedTail));
    }

    private LinkedList<Network> pruneSpanningCidrHead(Network head, Ip firstIp) {
        final Ip previousIp = firstIp.previous();
        final LinkedList<Network> result = new LinkedList<>();
        final List<Network> remainder = sorter.perform(new SubtractIpFromNetwork().perform(head, previousIp));
        boolean firstFound = false;
        for (Network cidr : remainder) {
            if (cidr.firstIp().equals(firstIp)) {
                firstFound = true;
            }
            if (firstFound) {
                result.add(cidr);
            }
        }
        return result;
    }

    private LinkedList<Network> pruneSpanningCidrTail(Network tail, Ip lastIp) {
        final Ip nextIp = lastIp.next();
        final LinkedList<Network> result = new LinkedList<>();
        final List<Network> remainder = sorter.perform(new SubtractIpFromNetwork().perform(tail, nextIp));
        for (Network cidr : remainder) {
            result.add(cidr);
            if (cidr.lastIp().equals(lastIp)) {
                break;
            }
        }
        return result;
    }
}
