package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv4Mask;
import org.junit.Test;

public class Ipv4MaskFromStringTest {

    @Test
    public void deserializingYieldsExpected() throws IOException {
        final String serialized = "{'netmask':'255.255.255.0'}".replace("'", "\"");
        final Ipv4Mask expected = Ipv4Mask.net(24);
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv4Mask got = mapper.readValue(serialized, BeanWithIpv4Mask.class);
        Assert.assertEquals(expected, got.getNetmask());
    }

}
