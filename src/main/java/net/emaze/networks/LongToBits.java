package net.emaze.networks;

import java.util.List;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.Dispatching;
import net.emaze.dysfunctional.Filtering;
import net.emaze.dysfunctional.Logic;
import net.emaze.dysfunctional.Reductions;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.dispatching.delegates.Identity;
import net.emaze.dysfunctional.dispatching.logic.Predicate;


public class LongToBits implements Delegate<Integer, Long> {
    
    private final Predicate<Boolean> isTrue = Dispatching.predicate(new Identity<Boolean>());
    private final Predicate<Boolean> isFalse = Logic.not(isTrue);
    
    @Override
    public Integer perform(Long longForm) {
        dbc.precondition(longForm != null, "longForm cannot be null");
        final List<Boolean> bits =  Consumers.all(Filtering.takeLast(32, new LongBitsIterator(longForm)));
        final int onesCount = (int)Reductions.count(Filtering.takeWhile(bits, isTrue));
        dbc.precondition(Reductions.every(Filtering.takeLast(32 - onesCount, bits), isFalse), "Noncontinuous '1' bits present");
        return onesCount;
    }

}
