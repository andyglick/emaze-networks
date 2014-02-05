package net.emaze.networks;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.Multiplexing;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.order.Order;

public class Ipv4RangeToNetworks implements BinaryDelegate<List<Ipv4Network>, Ipv4, Ipv4> {

    private final Ipv4SortNetworksByFirstThenLastIp sorter = new Ipv4SortNetworksByFirstThenLastIp();

    @Override
    public List<Ipv4Network> perform(Ipv4 firstIp, Ipv4 lastIp) {
        dbc.precondition(firstIp != null, "firstIp cannot be null");
        dbc.precondition(lastIp != null, "lastIp cannot be null");
        dbc.precondition(Order.of(firstIp.compareTo(lastIp)) != Order.GT, "lastIp cannot be lesser than firstIp");
        // Obtain a spanning cidr containing entire range
        final Ipv4Network spanningCidr = new Ipv4RangeToSpanningNetwork().perform(firstIp, lastIp);
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
        final LinkedList<Ipv4Network> prunedHead = pruneSpanningCidrHead(spanningCidr, firstIp);
        final List<Ipv4Network> prunedTail = pruneSpanningCidrTail(prunedHead.removeLast(), lastIp);
        return Consumers.all(Multiplexing.flatten(prunedHead, prunedTail));
    }

    private LinkedList<Ipv4Network> pruneSpanningCidrHead(Ipv4Network head, Ipv4 firstIp) {
        final Ipv4 previousIp = firstIp.previous();
        final LinkedList<Ipv4Network> result = new LinkedList<>();
        final List<Ipv4Network> remainder = sorter.perform(new Ipv4SubtractIpFromNetwork().perform(head, previousIp));
        boolean firstFound = false;
        for (Ipv4Network cidr : remainder) {
            if (cidr.firstIp().equals(firstIp)) {
                firstFound = true;
            }
            if (firstFound) {
                result.add(cidr);
            }
        }
        return result;
    }

    private LinkedList<Ipv4Network> pruneSpanningCidrTail(Ipv4Network tail, Ipv4 lastIp) {
        final Ipv4 nextIp = lastIp.next();
        final LinkedList<Ipv4Network> result = new LinkedList<>();
        final List<Ipv4Network> remainder = sorter.perform(new Ipv4SubtractIpFromNetwork().perform(tail, nextIp));
        for (Ipv4Network cidr : remainder) {
            result.add(cidr);
            if (cidr.lastIp().equals(lastIp)) {
                break;
            }
        }
        return result;
    }
}