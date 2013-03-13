package net.emaze.networks;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.tuples.Pair;

public class SubtractIpFromCidr implements BinaryDelegate<Set<Cidr>, Cidr, Ipv4> {

    @Override
    public Set<Cidr> perform(Cidr minuend, Ipv4 subtrahend) {
        return recursivelySubtract(minuend, subtrahend);
    }

    private Set<Cidr> recursivelySubtract(Cidr minuend, Ipv4 subtrahend) {
        if (!minuend.contains(subtrahend)) {
            return Collections.singleton(minuend);
        }
        if (minuend.netmask().equals(Mask.NARROWEST)){
            return Collections.emptySet();
        }
        final Set<Cidr> reminder = new HashSet<>();
        final Pair<Cidr, Cidr> split = minuend.split();
        if (split.first().contains(subtrahend)) {
            reminder.add(split.second());
            reminder.addAll(recursivelySubtract(split.first(), subtrahend));
        } else {
            reminder.add(split.first());
            reminder.addAll(recursivelySubtract(split.second(), subtrahend));
        }
        return reminder;
    }
}
